{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Network.BlockFetch.Client (
    -- * Block fetch protocol client implementation
    blockFetchClient,
    FetchClientPolicy(..),
    TraceFetchClientEvent(..),
    FetchClientStateVars,
  ) where

import           Control.Monad (unless)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Exception (assert)
import           Control.Tracer (Tracer, traceWith)

import           Ouroboros.Network.Block

import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined

import qualified Ouroboros.Network.ChainFragment as ChainFragment
import           Ouroboros.Network.ChainFragment (ChainFragment)
import           Ouroboros.Network.BlockFetch.ClientState
                   ( FetchClientStateVars, PeerFetchStatus(..)
                   , PeerFetchInFlight(..)
                   , FetchRequest(..)
                   , acknowledgeFetchRequest
                   , completeBlockDownload
                   , completeFetchBatch )
import           Ouroboros.Network.BlockFetch.DeltaQ
                   ( PeerGSV(..), SizeInBytes
                   , PeerFetchInFlightLimits(..) )


-- TODO #468 extract this from BlockFetchConsensusInterface
data FetchClientPolicy header block m =
     FetchClientPolicy {
       blockFetchSize     :: header -> SizeInBytes,
       blockMatchesHeader :: header -> block -> Bool,
       addFetchedBlock    :: Point block -> block -> m ()
     }


data BlockFetchProtocolFailure =
       BlockFetchProtocolFailureTooFewBlocks
     | BlockFetchProtocolFailureTooManyBlocks
     | BlockFetchProtocolFailureWrongBlock
     | BlockFetchProtocolFailureInvalidBody
  deriving (Eq, Show)

instance Exception BlockFetchProtocolFailure

data TraceFetchClientEvent header =
       AcknowledgedFetchRequest
         (FetchRequest header)
     | CompletedBlockFetch
         (Point header)
         (PeerFetchInFlight header)
          PeerFetchInFlightLimits
         (PeerFetchStatus header)
  deriving Show

-- | The implementation of the client side of block fetch protocol designed to
-- work in conjunction with our fetch logic.
--
blockFetchClient :: forall header block m.
                    (MonadSTM m, MonadTime m, MonadThrow m,
                     HasHeader header, HasHeader block,
                     HeaderHash header ~ HeaderHash block)
                 => Tracer m (TraceFetchClientEvent header)
                 -> FetchClientPolicy header block m
                 -> FetchClientStateVars m header
                 -> PeerPipelined (BlockFetch header block) AsClient BFIdle m ()
blockFetchClient tracer
                 FetchClientPolicy {
                   blockFetchSize,
                   blockMatchesHeader,
                   addFetchedBlock
                 }
                 stateVars =
    PeerPipelined (senderAwait Zero)
  where
    senderIdle :: forall n.
                  Nat n
               -> PeerSender (BlockFetch header block) AsClient
                             BFIdle n () m ()

    -- We have no requests to send. Check if we have any pending pipelined
    -- results to collect. If so, go round and collect any more. If not, block
    -- and wait for some new requests.
    senderIdle (Succ outstanding) =
      SenderCollect (Just (senderAwait (Succ outstanding)))
                    (\_ -> senderIdle outstanding)

    -- And similarly if there are no pending pipelined results at all.
    senderIdle Zero = senderAwait Zero
    --TODO: assert nothing in flight here

    senderAwait :: forall n.
                   Nat n
                -> PeerSender (BlockFetch header block) AsClient
                              BFIdle n () m ()
    senderAwait outstanding =
      SenderEffect $ do
      -- Atomically grab our next request and update our tracking state.
      -- We have now accepted this request.
      --
      -- It is important to note that we only update our tracking state when
      -- we /accept/ the request, not when the fetch logic /sets/ the request.
      -- The fetching logic can update the request up until the point where
      -- we accept it here. From here on the request is considered to be
      -- in-flight, and the tracking state that the fetch logic uses now
      -- reflects that.
      --
      (request, gsvs, inflightlimits) <- acknowledgeFetchRequest stateVars

      traceWith tracer (AcknowledgedFetchRequest request)

      return $ senderActive outstanding gsvs inflightlimits
                            (fetchRequestFragments request)

    senderActive :: forall n.
                    Nat n
                 -> PeerGSV
                 -> PeerFetchInFlightLimits
                 -> [ChainFragment header]
                 -> PeerSender (BlockFetch header block) AsClient
                               BFIdle n () m ()

    -- We now do have some requests that we have accepted but have yet to
    -- actually send out. Lets send out the first one.
    senderActive outstanding gsvs inflightlimits (fragment:fragments) =
      SenderEffect $ do
{-
        now <- getMonotonicTime
        --TODO: should we pair this up with the senderAwait earlier?
        inFlight  <- readTVar fetchClientInFlightVar

        let blockTrailingEdges =
              blockArrivalShedule
                gsvs
                inFlight
                (map snd fragment)

        timeout <- newTimeout (head blockTrailingEdges)
        fork $ do
          fired <- awaitTimeout timeout
          when fired $
            atomically (writeTVar _ PeerFetchStatusAberrant)
-}
        let range = assert (not (ChainFragment.null fragment)) $
                    ChainRange (blockPoint lower)
                               (blockPoint upper)
              where
                Just lower = ChainFragment.last fragment
                Just upper = ChainFragment.head fragment

        return $
          SenderPipeline
            (ClientAgency TokIdle)
            (MsgRequestRange range)
            (receiverBusy (ChainFragment.toOldestFirst fragment) inflightlimits)
            (senderActive (Succ outstanding) gsvs inflightlimits fragments)

    -- And when we run out, go back to idle.
    senderActive outstanding _ _ [] = senderIdle outstanding


    receiverBusy :: [header]
                 -> PeerFetchInFlightLimits
                 -> PeerReceiver (BlockFetch header block) AsClient
                                 BFBusy BFIdle m ()
    receiverBusy headers inflightlimits =
      ReceiverAwait
        (ServerAgency TokBusy) $ \msg ->
        case msg of
          -- The server is reporting that the range we asked for does not exist.
          -- This can happen (even if we didn't make any mistakes) if their
          -- chain forked in the time between when they told us and when we
          -- asked for this range of blocks. If this happens, it should
          -- certainly be the case that this peer doesn't continue to tell us
          -- that this range of blocks is in their chain.
          --
          -- FIXME: For now we will not do the detailed error checking to check
          -- that the peer is not cheating us. Nor will we track these failure
          -- points to make sure we do not ask for extensions of this again.
          MsgNoBlocks   -> ReceiverDone ()
          --TODO: also adjust the in-flight stats

          MsgStartBatch -> receiverStreaming inflightlimits headers

    receiverStreaming :: PeerFetchInFlightLimits
                      -> [header]
                      -> PeerReceiver (BlockFetch header block) AsClient
                                      BFStreaming BFIdle m ()
    receiverStreaming inflightlimits headers =
      ReceiverAwait
        (ServerAgency TokStreaming) $ \msg ->
        case (msg, headers) of
          (MsgBatchDone, []) -> ReceiverEffect $ do
            completeFetchBatch stateVars
            return (ReceiverDone ())


          (MsgBlock block, header:headers') -> ReceiverEffect $ do
            --TODO: consider how to enforce expected block size limit.
            -- They've lied and are sending us a massive amount of data.
            -- Resource consumption attack.

{-
            -- Now it's totally possible that the timeout already fired
            -- if not, we can update it, making sure the delay is > 0
            now <- getMonotonicTime
            updateTimeout timeout (diffTime now )
-}

            unless (blockPoint header == castPoint (blockPoint block)) $
              throwM BlockFetchProtocolFailureWrongBlock

            -- This is moderately expensive.
            unless (blockMatchesHeader header block) $
              throwM BlockFetchProtocolFailureInvalidBody

            -- write it to the volatile block store
            --FIXME: this is not atomic wrt the in-flight and status updates
            -- above. This would allow a read where the block is no longer
            -- in-flight but is still not in the fetched block store.
            -- either 1. make it atomic, or 2. do this first, or 3. some safe
            -- interleaving

            -- Add the block to the chain DB, notifying of any new chains.
            addFetchedBlock (castPoint (blockPoint header)) block

            -- Note that we add the block to the chain DB /before/ updating our
            -- current status and in-flight stats. Otherwise blocks will
            -- disappear from our in-flight set without yet appearing in the
            -- fetched block set. The fetch logic would conclude it has to
            -- download the missing block(s) again.

            -- Update our in-flight stats and our current status
            (inflight, currentStatus) <-
              completeBlockDownload blockFetchSize inflightlimits
                                    header stateVars

            traceWith tracer $ CompletedBlockFetch
                                 (blockPoint header)
                                 inflight inflightlimits
                                 currentStatus

            return (receiverStreaming inflightlimits headers')

          (MsgBatchDone, (_:_)) -> ReceiverEffect $
            throwM BlockFetchProtocolFailureTooFewBlocks

          (MsgBlock _, []) -> ReceiverEffect $
            throwM BlockFetchProtocolFailureTooManyBlocks


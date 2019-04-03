{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Network.Mux (
      MiniProtocolDescription (..)
    , MiniProtocolDescriptions
    , MiniProtocolLimits (..)
    , ProtocolEnum (..)
    , MiniProtocolId (..)
    , MiniProtocolMode (..)
    , MuxBearerState (..)
    , MuxError (..)
    , MuxErrorType (..)
    , MuxStyle (..)
    , MuxSDU (..)
    , MuxVersion
    , RemoteClockModel (..)
    , encodeMuxSDU
    , decodeMuxSDUHeader
    , muxBearerSetState
    , muxStart
    ) where

import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Write (toLazyByteString)
import           Control.Monad
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import qualified Control.Monad.Fail as Fail
import           Data.Array
import qualified Data.ByteString.Lazy as BL
import           Data.List (intersect)
import           Data.Maybe (catMaybes)
import           GHC.Stack
import           Text.Printf

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Mux.Control
import           Ouroboros.Network.Mux.Egress
import           Ouroboros.Network.Mux.Ingress
import           Ouroboros.Network.Mux.Types

-- | muxStart starts a mux bearer for the specified protocols corresponding to
-- one of the provided Versions.
-- TODO: replace MonadSay with iohk-monitoring-framework.
muxStart :: forall m ptcl.
            ( MonadAsync m, MonadSay m, MonadSTM m, MonadThrow m
            , Ord ptcl, Enum ptcl, Bounded ptcl, Show ptcl, MiniProtocolLimits ptcl)
         => [SomeVersion]
         -> (SomeVersion -> Maybe (MiniProtocolDescriptions ptcl m))
         -> MuxBearer ptcl m
         -> MuxStyle
         -> Maybe (Maybe SomeException -> m ())  -- Optional callback for result
         -> m ()
muxStart versions mpds bearer style rescb_m = do
    tbl <- setupTbl
    tq <- atomically $ newTBQueue 100
    cnt <- newTVarM 0
    udesc <- case style of
                       StyleClient -> Just <$> muxClient versions mpds bearer
                       StyleServer -> muxServer versions mpds bearer

    let pmss = PerMuxSS tbl tq bearer
        jobs = [ demux pmss
               , mux cnt pmss
               , muxControl pmss ModeResponder
               , muxControl pmss ModeInitiator
               ]
    mjobs <- sequence [ mpsJob cnt pmss udesc ptcl
                      | ptcl <- [minBound..maxBound] ]
    aids <- mapM async $ jobs ++ concat mjobs
    muxBearerSetState bearer Mature
    watcher aids

  where
    watcher :: [Async m a] -> m ()
    watcher as = do
        (_,r) <- waitAnyCatchCancel as
        close bearer
        muxBearerSetState bearer Dead
        case r of
          Left e -> do
            case rescb_m of
                 Nothing -> say $ "Mux Bearer died due to " ++ show e
                 Just _  -> return ()
            sequence_ $ rescb_m <*> Just (Just e)
          Right _ ->
            sequence_ $ rescb_m <*> Just Nothing


    -- Construct the array of TBQueues, one for each protocol id, and each mode
    setupTbl :: m (MiniProtocolDispatch ptcl m)
    setupTbl = MiniProtocolDispatch
            -- cover full range of type (MiniProtocolId ptcl, MiniProtocolMode)
             . array (minBound, maxBound)
           <$> sequence [ do q <- atomically (newTVar BL.empty)
                             return ((ptcl, mode), q)
                        | ptcl <- [minBound..maxBound]
                        , mode <- [ModeInitiator, ModeResponder] ]

    mpsJob
      :: TVar m Int
      -> PerMuxSharedState ptcl m
      -> Maybe (MiniProtocolDescriptions ptcl m)
      -> ptcl
      -> m [m ()]
    mpsJob _   _    Nothing      _     = pure []
    mpsJob cnt pmss (Just udesc) mpdId = do
        let mpd = udesc mpdId
        w_i <- atomically newEmptyTMVar
        w_r <- atomically newEmptyTMVar

        let initiatorChannel :: Channel m BL.ByteString
            initiatorChannel = muxChannel pmss
                                (AppProtocolId mpdId)
                                ModeInitiator
                                w_i cnt

            responderChannel :: Channel m BL.ByteString
            responderChannel = muxChannel pmss
                                (AppProtocolId mpdId)
                                ModeResponder
                                w_r cnt

        return $ map (>> mpsJobExit cnt) $ catMaybes
          [ mpdInitiator mpd <*> Just initiatorChannel
                
          , mpdResponder mpd <*> Just responderChannel
          ]

    -- cnt represent the number of SDUs that are queued but not yet sent.  Job
    -- threads will be prevented from exiting until all SDUs have been
    -- transmitted unless an exception/error is encounter. In that case all
    -- jobs will be cancelled directly.
    mpsJobExit :: TVar m Int -> m ()
    mpsJobExit cnt = do
        muxBearerSetState bearer Dying
        atomically $ do
            c <- readTVar cnt
            unless (c == 0) retry

muxControl :: ( HasCallStack, MonadSTM m, MonadSay m, MonadThrow m, Ord ptcl, Enum ptcl
              , MiniProtocolLimits ptcl)
           => PerMuxSharedState ptcl m
           -> MiniProtocolMode
           -> m ()
muxControl pmss md = do
    _ <- atomically $ do
        buf <- readTVar (ingressQueue (dispatchTable pmss) Muxcontrol md)
        when (buf == BL.empty)
            retry
    throwM $ MuxError MuxControlProtocolError "MuxControl message on mature MuxBearer" callStack

data AwaitReply = AwaitReply

decodeXXX = do
      n   <- CBOR.decodeListLen
      if n == 1
          then do
              k <- CBOR.decodeWord
              if k == 1
                  then return AwaitReply
                  else Fail.fail "uninteresting data"
          else Fail.fail "uninteresting data"


-- Look for MsgAwaitReply
hackXXX :: BL.ByteString -> Bool
hackXXX bs =
    case CBOR.deserialiseFromBytes decodeXXX bs :: Either CBOR.DeserialiseFailure (BL.ByteString, AwaitReply) of
         Left _ -> False
         Right (left, _) -> BL.empty == left


-- | muxChannel creates a duplex channel for a specific 'MiniProtocolId' and 'MiniProtocolMode'.
muxChannel :: ( MonadSTM m, MonadSay m, MonadThrow m, Ord ptcl, Enum ptcl, Show ptcl
              , MiniProtocolLimits ptcl, HasCallStack) =>
    PerMuxSharedState ptcl m ->
    MiniProtocolId ptcl ->
    MiniProtocolMode ->
    TMVar m BL.ByteString ->
    TVar m Int ->
    Channel m BL.ByteString
muxChannel pmss mid md w cnt =
    Channel {send, recv}
  where
    send encoding = do
        -- We send CBOR encoded messages by encoding them into by ByteString
        -- forwarding them to the 'mux' thread, see 'Desired servicing semantics'.
        --say $ printf "send mid %s mode %s" (show mid) (show md)
        when (BL.length encoding > maximumMessageSize mid) $
            throwM $ MuxError MuxTooLargeMessage
                (printf "Attempting to send a message of size %d on %s %s" (BL.length encoding)
                        (show mid) (show $ md))
                callStack
        atomically $ modifyTVar' cnt (+ 1)
        atomically $ putTMVar w encoding
        atomically $ writeTBQueue (tsrQueue pmss) (TLSRDemand mid md (Wanton w))
    recv = do
        -- We receive CBOR encoded messages as ByteStrings (possibly partial) from the
        -- matching ingress queueu. This is the same queue the 'demux' thread writes to.
        blob <- atomically $ do
            let q = ingressQueue (dispatchTable pmss) mid md
            blob <- readTVar q
            -- Incase of a MsgAwaitReply we wait so that we can deliver MsgRollForward or
            -- MsgRollBackward in the same blob.
            let hack = if hackXXX blob
                           then True
                           else False
            if blob == BL.empty || hack
                then retry
                else writeTVar q BL.empty >> return blob
        -- say $ printf "recv mid %s mode %s blob len %d" (show mid) (show md) (BL.length blob)
        if BL.null blob
           then pure Nothing
           else do
               when (BL.length blob == 56) $
                   say "triggering bug"
               return $ Just blob

muxBearerSetState :: (MonadSTM m, Ord ptcl, Enum ptcl, Bounded ptcl)
                  => MuxBearer ptcl m
                  -> MuxBearerState
                  -> m ()
muxBearerSetState bearer newState = atomically $ writeTVar (state bearer) newState

-- | Initiate version negotiation with the peer the MuxBearer is connected to
muxClient :: (MonadAsync m, MonadSay m, MonadSTM m, MonadThrow m,
            Ord ptcl, Enum ptcl, Bounded ptcl, HasCallStack)
        => [SomeVersion]
        -> (SomeVersion -> Maybe (MiniProtocolDescriptions ptcl m))
        -> MuxBearer ptcl m
        -> m (MiniProtocolDescriptions ptcl m)
muxClient versions mpds_fn bearer = do
    let msg = MsgInitReq versions
        blob = toLazyByteString $ encodeControlMsg msg
        sdu = MuxSDU (RemoteClockModel 0) Muxcontrol ModeInitiator (fromIntegral $ BL.length blob) blob

    void $ write bearer sdu
    (rsp, _) <- Ouroboros.Network.Mux.Types.read bearer
    if msId rsp /= Muxcontrol || msMode rsp /= ModeResponder
       then throwM $ MuxError MuxUnknownMiniProtocol "invalid muxInit rsp id or mode" callStack
       else do
           let rspMsg_e = CBOR.deserialiseFromBytes (decodeControlMsg versions) (msBlob rsp)
           case rspMsg_e of
                Left e -> throwM e
                Right (_, MsgInitRsp version) ->
                    -- verify that rsp version matches one of our proposals
                    case mpds_fn version of
                         Nothing   -> throwM $ MuxError MuxControlProtocolError
                                               "muxInit invalid version selected" callStack
                         Just mpds -> return mpds
                Right (_, MsgInitReq _) -> throwM $ MuxError MuxControlProtocolError
                                                    "muxInit response as request" callStack
                Right (_, MsgInitFail e) -> throwM $ MuxError MuxControlNoMatchingVersion e callStack

-- | Wait for the connected peer to initiate version negotiation.
muxServer :: (MonadAsync m, MonadSay m, MonadSTM m, MonadThrow m,
             Ord ptcl, Enum ptcl, Bounded ptcl, HasCallStack)
          => [SomeVersion]
          -> (SomeVersion -> Maybe (MiniProtocolDescriptions ptcl m))
          -> MuxBearer ptcl m
          -> m (Maybe (MiniProtocolDescriptions ptcl m))
muxServer localVersions mpds_fn bearer = do
    (req, _) <- Ouroboros.Network.Mux.Types.read bearer
    if msId req /= Muxcontrol || msMode req /= ModeInitiator
       then throwM $ MuxError MuxUnknownMiniProtocol "invalid muxInit req id or mode" callStack
       else do
           let reqMsg_e = CBOR.deserialiseFromBytes (decodeControlMsg localVersions) (msBlob req)
           case reqMsg_e of
                Left e -> throwM e
                Right (_, MsgInitReq remoteVersions) -> do
                    let matchingVersions = localVersions `intersect` remoteVersions
                    if null matchingVersions
                       then do
                           sndSdu $ MsgInitFail $ "No matching version, try " ++
                                show (last localVersions)

                           throwM $ MuxError MuxControlNoMatchingVersion "no matching version" callStack
                       else do
                           let version = maximum matchingVersions
                               msg  = MsgInitRsp version
                           sndSdu msg
                           return (mpds_fn version)

                Right (_, MsgInitRsp _) -> throwM $ MuxError MuxControlProtocolError
                                                    "muxInit request as response" callStack
                Right (_, MsgInitFail _) -> throwM $ MuxError MuxControlProtocolError
                                                     "muxInit fail in request" callStack

  where
    sndSdu msg = do
        let blob = toLazyByteString $ encodeControlMsg msg
            sdu = MuxSDU (RemoteClockModel 0) Muxcontrol ModeResponder
                         (fromIntegral $ BL.length blob) blob
        void $ write bearer sdu


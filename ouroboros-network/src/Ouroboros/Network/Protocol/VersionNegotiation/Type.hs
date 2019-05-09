{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Ouroboros.Network.Protocol.VersionNegotiation.Type
  (
    VersionNegotiationError (..)

  -- * Version Negotiation Protocol
  , VersionNegotiationProtocol (..)
  , Message (..)
  , ClientHasAgency (..)
  , ServerHasAgency (..)
  , NobodyHasAgency (..)

  -- ** Version Negotiation client
  , versionNegotiationClientPeer
  -- ** Version Negotiation server
  , versionNegotiationServerPeer

  -- ** Tests
  , pureVersionNegotiation
  )
  where


import           Data.Typeable (Typeable, cast)
import           Data.List (intersect)
import           Data.Map (Map)
import qualified Data.Map as Map

import qualified Codec.CBOR.Term     as CBOR

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Protocol.VersionNegotiation.Version

-- |
-- State transitions of the version negotation protocol.
--
data VersionNegotiationProtocol vNumber vBlob where
    StPropose :: VersionNegotiationProtocol vNumber vBlob
    StConfirm :: VersionNegotiationProtocol vNumber vBlob
    StDone    :: VersionNegotiationProtocol vNumber vBlob


instance Protocol (VersionNegotiationProtocol vNumber vBlob) where

    data Message (VersionNegotiationProtocol vNumber vBlob) from to where

      -- |
      -- Propose a list of versions together with version data.  It must be
      -- encoded to a sorted list.
      --
      MsgProposeVersions
        :: Map vNumber vBlob
        -> Message (VersionNegotiationProtocol vNumber vBlob) StPropose StConfirm

      -- |
      -- The remote end decide which version to use and sends it back:
      --
      MsgAcceptVersion
        :: vNumber
        -> vBlob
        -> Message (VersionNegotiationProtocol vNumber vBlob) StConfirm StDone

      -- |
      -- or it refuses to run any version,
      --
      MsgRefuse
        :: Message (VersionNegotiationProtocol vNumber vBlob) StConfirm StDone

      -- |
      -- or a give the client an explicit message that the version was not
      -- recognised.
      --
      MsgUnknownVersion
        :: Message (VersionNegotiationProtocol vNumber vBlob) StConfirm StDone

    data ClientHasAgency st where
      TokPropose :: ClientHasAgency StPropose

    data ServerHasAgency st where
      TokConfirm :: ServerHasAgency StConfirm

    data NobodyHasAgency st where
      TokDone    :: NobodyHasAgency StDone

    exclusionLemma_ClientAndServerHaveAgency TokPropose tok = case tok of {}
    exclusionLemma_NobodyAndClientHaveAgency TokDone    tok = case tok of {}
    exclusionLemma_NobodyAndServerHaveAgency TokDone    tok = case tok of {}

deriving instance (Show vNumber, Show vBlob)
    => Show (Message (VersionNegotiationProtocol vNumber vBlob) from to)

-- |
-- Version negotiation failures
data VersionNegotiationError =
    -- |
    -- Remote peer rejected version
      VersionNegotiationRejected

    -- |
    -- Protocol error, remote peer accepted a version but send back unknown version.
    --
    | VersionNegotiationUnknownVersion

    -- | Error while decoding version data on agreed version.
    --
    | VersionNegotiationDecodeError

    -- |
    -- Protocol error, remote peer accepted a version but send wrong associated
    -- data type.
    --
    | VersionNegotiationTypeCastError

  deriving (Eq, Ord, Show)

encodeVersions
  :: forall vNumber extra r vBlob.
     (forall vData. extra vData -> vData -> vBlob)
  -> Versions vNumber extra r
  -> Map vNumber vBlob
encodeVersions encoder (Versions vs) = go <$> vs
    where
      go :: Sigma (Version extra r) -> vBlob
      go (Sigma vData Version {versionExtra}) = encoder versionExtra vData

-- |
-- Version negotiation client which offers @'KnownVersions' vNumber r@ to the remote peer.
--
versionNegotiationClientPeer
  :: Ord vNumber
  => (forall vData. extra vData -> vData -> CBOR.Term)
  -> (forall vData. extra vData -> CBOR.Term -> Maybe vData)
  -> Versions vNumber extra r
  -> Peer (VersionNegotiationProtocol vNumber CBOR.Term) AsClient StPropose m (Either VersionNegotiationError r)
versionNegotiationClientPeer encodeData decodeData versions =
  -- send known versions
  Yield (ClientAgency TokPropose) (MsgProposeVersions $ encodeVersions encodeData versions) $

    Await (ServerAgency TokConfirm) $ \msg -> case msg of

      -- the server refused common highest version
      MsgRefuse ->
        Done TokDone (Left VersionNegotiationRejected)

      -- the server didn't recognised the common highest version; either the
      -- intersection was empty all the decoding failed.
      MsgUnknownVersion ->
        Done TokDone (Left VersionNegotiationUnknownVersion)

      -- the server accepted a version, sent back the version number and its
      -- version data blob
      MsgAcceptVersion vNumber vBlob ->
        case vNumber `Map.lookup` getVersions versions of
          Nothing -> Done TokDone (Left VersionNegotiationUnknownVersion)
          Just (Sigma vData version) ->
            case decodeData (versionExtra version) vBlob of

              Nothing ->
                Done TokDone (Left VersionNegotiationDecodeError)

              Just vData' ->
                Done TokDone $ Right $ runApplication (versionApplication version) vData vData'

-- |
-- Version negotiation server which accepts highest version offered by the more
-- peer that also belongs to @knownVersion@.
--
versionNegotiationServerPeer
  :: Ord vNumber
  => (forall vData. extra vData -> vData -> vBlob)
  -> (forall vData. extra vData -> vBlob -> Maybe vData)
  -> (forall vData. extra vData -> vData -> vData -> Bool)
  -> Versions vNumber extra r
  -> Peer (VersionNegotiationProtocol vNumber vBlob) AsServer StPropose m (Either VersionNegotiationError r)
versionNegotiationServerPeer encodeData decodeData acceptVersion versions =
    -- await for versions proposed by a client
    Await (ClientAgency TokPropose) $ \msg -> case msg of

      MsgProposeVersions vMap ->
        -- compute intersection of local and remote versions
        case map fst (Map.toDescList vMap) `intersect` map fst (Map.toDescList (getVersions versions)) of
          [] ->
            Yield (ServerAgency TokConfirm)
                  MsgUnknownVersion
                  (Done TokDone (Left VersionNegotiationUnknownVersion))

          vNumber:_ ->
            case (getVersions versions Map.! vNumber, vMap Map.! vNumber) of
            (Sigma vData version, vBlob) ->
              case decodeData (versionExtra version) vBlob of
                Nothing ->
                  Yield (ServerAgency TokConfirm)
                        MsgUnknownVersion
                        (Done TokDone $ Left $ VersionNegotiationDecodeError)

                Just vData' ->
                  if acceptVersion (versionExtra version) vData vData'

                    -- We agree on the version; send back the aggreed version
                    -- number @vNumber@ and encoded data associated with our
                    -- version.
                    then Yield (ServerAgency TokConfirm)
                               (MsgAcceptVersion vNumber (encodeData (versionExtra version) vData))
                               (Done TokDone $ Right $ runApplication (versionApplication version) vData vData')

                    -- We disagree on the version.
                    else Yield (ServerAgency TokConfirm)
                               MsgRefuse
                               (Done TokDone $ Left $ VersionNegotiationRejected)

-- |
-- Pure computation which serves as a reference impementation of the
-- @'VersionNegotiationProtocol'@ protocol. Useful for testing
-- @'versionNegotiationClientPeer'@ against @'versionNegotiationServerPeer'@
-- using  @'connect'@
--
pureVersionNegotiation
  ::forall vNumber extra r. Ord vNumber
  => (forall vData. extra vData -> Dict Typeable vData)
  -> (forall vData. extra vData -> vData -> vData -> Bool)
  -> Versions vNumber extra r -- reponders's \/ server's known versions
  -> Versions vNumber extra r -- initiator's \/ client's known versions
  -> (Maybe r, Maybe r)
pureVersionNegotiation isTypeable acceptVersion (Versions serverVersions) (Versions clientVersions) =

      case Map.toDescList $ serverVersions `Map.intersection` clientVersions of

        []             -> (Nothing, Nothing)

        (vNumber, _):_ -> 
          case (serverVersions Map.! vNumber, clientVersions Map.! vNumber) of

            ( Sigma vData version, Sigma vData' version' ) ->
                  case (isTypeable (versionExtra version), isTypeable (versionExtra version')) of
                        (Dict, Dict) -> case (cast vData, cast vData') of
                          (Just d, Just d') ->
                            ( if acceptVersion (versionExtra version) vData d'
                                then Just $ runApplication (versionApplication version) vData d'
                                else Nothing

                            , Just $ runApplication (versionApplication version') vData' d
                            )
                          _ -> (Nothing, Nothing)

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ouroboros.Network.Protocol.VersionNegotiation.Codec
  ( codecVersionNegotiation
  , SerialiseTerm (..)
  ) where

import           Control.Monad.Class.MonadST
import           Control.Monad (replicateM)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Term     as CBOR
import           Codec.Serialise (Serialise)
import qualified Codec.Serialise     as CBOR

import           Network.TypedProtocol.Codec hiding (encode, decode)
import           Ouroboros.Network.Codec (mkCodecCborLazyBS)

import           Ouroboros.Network.Protocol.VersionNegotiation.Type


class SerialiseTerm a where
  encodeTerm :: a -> CBOR.Term
  decodeTerm :: CBOR.Term -> Maybe a


codecVersionNegotiation
  :: forall vNumber m.
     ( Monad m
     , MonadST m
     , Ord vNumber
     , Enum vNumber
     , Serialise vNumber
     , Show vNumber
     )
  => Codec (VersionNegotiationProtocol vNumber CBOR.Term) CBOR.DeserialiseFailure m ByteString
codecVersionNegotiation = mkCodecCborLazyBS encode decode
    where
      encode :: forall (pr :: PeerRole) st st'.
                PeerHasAgency pr st
             -> Message (VersionNegotiationProtocol vNumber CBOR.Term) st st'
             -> CBOR.Encoding

      encode (ClientAgency TokPropose) (MsgProposeVersions vs) =
        let vs' = Map.assocs vs
        in
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 0
        <> CBOR.encodeMapLen (fromIntegral $ length vs')
        <> mconcat [    CBOR.encode vNumber
                     <> CBOR.encodeTerm vBlob 
                   | (vNumber, vBlob) <- vs'
                   ]

      encode (ServerAgency TokConfirm) (MsgAcceptVersion vNumber vBlob) =
           CBOR.encodeListLen 3
        <> CBOR.encodeWord 1
        <> CBOR.encode vNumber
        <> CBOR.encodeTerm vBlob

      encode (ServerAgency TokConfirm) MsgRefuse =
           CBOR.encodeListLen 1
        <> CBOR.encodeWord 2

      encode (ServerAgency TokConfirm) MsgUnknownVersion =
           CBOR.encodeListLen 1
        <> CBOR.encodeWord 3


      decode :: forall (pr :: PeerRole) s (st :: VersionNegotiationProtocol vNumber CBOR.Term).
                PeerHasAgency pr st
             -> CBOR.Decoder s (SomeMessage st)
      decode stok = do
        _ <- CBOR.decodeListLen
        key <- CBOR.decodeWord
        case (stok, key) of
          (ClientAgency TokPropose, 0) -> do
            l  <- CBOR.decodeMapLen
            vs <- replicateM (fromIntegral l) ((,) <$> CBOR.decode <*> CBOR.decodeTerm)
            pure $ SomeMessage $ MsgProposeVersions (Map.fromList vs)
          (ServerAgency TokConfirm, 1) ->
            SomeMessage <$> (MsgAcceptVersion <$> CBOR.decode <*> CBOR.decodeTerm)
          (ServerAgency TokConfirm, 2) -> pure (SomeMessage MsgRefuse)
          (ServerAgency TokConfirm, 3) -> pure (SomeMessage MsgUnknownVersion)

          (ClientAgency TokPropose, _) -> fail "codecVersionNegotation.Propose: unexpected key"
          (ServerAgency TokConfirm, _) -> fail "codecVersionNegotation.Confirm: unexpected key"

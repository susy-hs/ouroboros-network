{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GADTs                 #-}

module Ouroboros.Network.Mux.VersionNegotiation where

import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Maybe (catMaybes)

import           Control.Monad (replicateM)

import           Ouroboros.Network.Mux.Types

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Term     as CBOR


class (Bounded (ProtocolVersion v),
       Enum    (ProtocolVersion v)) => ProtocolVersionInfo v where

  data ProtocolVersion v

  protocolVersion   :: v -> ProtocolVersion v

  encodeVersionInfo :: v -> CBOR.Encoding
  decodeVersionInfo :: ProtocolVersion v -> CBOR.Decoder s v


encodeVersionInfoMap :: ProtocolVersionInfo v => [v] -> CBOR.Encoding
encodeVersionInfoMap vs =
    CBOR.encodeMapLen (fromIntegral (length vs))
 <> mconcat [    CBOR.encodeWord ver
              <> encodeVersionInfo v
            | v <- vs
            , let ver = fromIntegral (fromEnum (protocolVersion v)) ]
 

decodeVersionInfoMap :: forall v s. ProtocolVersionInfo v => CBOR.Decoder s [v]
decodeVersionInfoMap = do
    len <- CBOR.decodeMapLen
    fmap catMaybes $ replicateM len $ do
      ver <- CBOR.decodeWord --TODO use word, and check for overflow
      if ver `Set.member` knownVersionsSet
        then Just <$> decodeVersionInfo (toEnum (fromIntegral ver))
        else CBOR.decodeTerm >> return Nothing
  where
    knownVersionsSet :: Set Word
    knownVersionsSet = Set.fromList (map (fromIntegral . fromEnum) knownVersions)

    knownVersions :: [ProtocolVersion v]
    knownVersions = [minBound .. maxBound]


data ProtocolVersionDescription m where
       ProtocolVersionDescription :: (Bounded ptcl, Enum ptcl)
                                  => ptcl
                                  -> MiniProtocolDescription ptcl m
                                  -> ProtocolVersionDescription m

type OverallProtocolDescription v m = v -> ProtocolVersionDescription m

data NodeToNodeVersionInfo =
       NodeToNodeV1 NetworkMagic
     | NodeToNodeV2 NetworkMagic SomethingElse

type NetworkMagic  = Word
type SomethingElse = Word

instance ProtocolVersionInfo NodeToNodeVersionInfo where

  data ProtocolVersion NodeToNodeVersionInfo =
           NodeToNodeVersion1
         | NodeToNodeVersion2
    deriving (Bounded, Enum)

  protocolVersion NodeToNodeV1{} = NodeToNodeVersion1
  protocolVersion NodeToNodeV2{} = NodeToNodeVersion2

  encodeVersionInfo (NodeToNodeV1 magic) =
    CBOR.encodeWord magic

  encodeVersionInfo (NodeToNodeV2 magic thing) =
       CBOR.encodeListLen 2
    <> CBOR.encodeWord magic
    <> CBOR.encodeWord thing

  decodeVersionInfo NodeToNodeVersion1 =
      NodeToNodeV1 <$> CBOR.decodeWord

  decodeVersionInfo NodeToNodeVersion2 = do
      CBOR.decodeListLenOf 2
      NodeToNodeV2 <$> CBOR.decodeWord <*> CBOR.decodeWord


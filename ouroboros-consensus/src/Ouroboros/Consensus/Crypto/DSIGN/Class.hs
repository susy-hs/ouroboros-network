{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Abstract digital signatures.
module Ouroboros.Consensus.Crypto.DSIGN.Class
    ( DSIGNAlgorithm (..)
    , SignedDSIGN (..)
    , signedDSIGN
    , verifySignedDSIGN
    ) where

import           Codec.Serialise.Encoding (Encoding)
import           Crypto.Random (MonadRandom)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Util.Condense

class ( Show (VerKeyDSIGN v)
      , Ord (VerKeyDSIGN v)
      , Show (SignKeyDSIGN v)
      , Ord (SignKeyDSIGN v)
      , Show (SigDSIGN v)
      , Condense (SigDSIGN v)
      , Ord (SigDSIGN v)
      )
      => DSIGNAlgorithm v where

    data VerKeyDSIGN v :: *
    data SignKeyDSIGN v :: *
    data SigDSIGN v :: *

    genKeyDSIGN :: MonadRandom m => m (SignKeyDSIGN v)
    deriveVerKeyDSIGN :: SignKeyDSIGN v -> VerKeyDSIGN v
    signDSIGN :: MonadRandom m => (a -> Encoding) -> a -> SignKeyDSIGN v -> m (SigDSIGN v)
    verifyDSIGN :: (a -> Encoding) -> VerKeyDSIGN v -> a -> SigDSIGN v -> Bool

newtype SignedDSIGN v a = SignedDSIGN (SigDSIGN v)
  deriving (Generic)

deriving instance DSIGNAlgorithm v => Show (SignedDSIGN v a)
deriving instance DSIGNAlgorithm v => Eq   (SignedDSIGN v a)
deriving instance DSIGNAlgorithm v => Ord  (SignedDSIGN v a)

instance Condense (SigDSIGN v) => Condense (SignedDSIGN v a) where
    condense (SignedDSIGN sig) = condense sig

signedDSIGN :: (DSIGNAlgorithm v, MonadRandom m)
            => (a -> Encoding) -> a -> SignKeyDSIGN v -> m (SignedDSIGN v a)
signedDSIGN encoder a key = SignedDSIGN <$> signDSIGN encoder a key

verifySignedDSIGN :: DSIGNAlgorithm v
                  => (a -> Encoding) -> VerKeyDSIGN v -> a -> SignedDSIGN v a -> Bool
verifySignedDSIGN encoder key a (SignedDSIGN s) = verifyDSIGN encoder key a s

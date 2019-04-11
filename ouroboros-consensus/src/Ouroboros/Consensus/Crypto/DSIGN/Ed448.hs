{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Ed448 digital signatures.
module Ouroboros.Consensus.Crypto.DSIGN.Ed448
    ( Ed448DSIGN
    ) where

import           Crypto.PubKey.Ed448
import           Data.ByteArray (ByteArrayAccess)
import           Data.Function (on)
import           GHC.Generics (Generic)

import           Ouroboros.Consensus.Crypto.DSIGN.Class
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Serialise (toBS)

data Ed448DSIGN

instance DSIGNAlgorithm Ed448DSIGN where

    newtype VerKeyDSIGN Ed448DSIGN = VerKeyEd448DSIGN PublicKey
        deriving (Show, Eq, Generic, ByteArrayAccess)

    newtype SignKeyDSIGN Ed448DSIGN = SignKeyEd448DSIGN SecretKey
        deriving (Show, Eq, Generic, ByteArrayAccess)

    newtype SigDSIGN Ed448DSIGN = SigEd448DSIGN Signature
        deriving (Show, Eq, Generic, ByteArrayAccess)

    genKeyDSIGN = SignKeyEd448DSIGN <$> generateSecretKey

    deriveVerKeyDSIGN (SignKeyEd448DSIGN sk) = VerKeyEd448DSIGN $ toPublic sk

    signDSIGN toEnc a (SignKeyEd448DSIGN sk) = do
        let vk = toPublic sk
            bs = toBS $ toEnc a
        return $ SigEd448DSIGN $ sign sk vk bs

    verifyDSIGN toEnc (VerKeyEd448DSIGN vk) a (SigEd448DSIGN sig) = verify vk (toBS $ toEnc a) sig

instance Ord (VerKeyDSIGN Ed448DSIGN) where
    compare = compare `on` show

instance Ord (SignKeyDSIGN Ed448DSIGN) where
    compare = compare `on` show

instance Ord (SigDSIGN Ed448DSIGN) where
    compare = compare `on` show

instance Condense (SigDSIGN Ed448DSIGN) where
    condense (SigEd448DSIGN s) = show s

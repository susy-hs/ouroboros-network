{-# LANGUAGE DuplicateRecordFields   #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}

{-# OPTIONS_ghc -fno-warn-orphans #-}

module Ouroboros.Consensus.Ledger.Byron where

import           Cardano.Binary (Annotated(..))
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Delegation.Validation.Activation as CC.Delegation
import qualified Cardano.Chain.Delegation.Validation.Interface as CC.Delegation
import qualified Cardano.Chain.Update.Validation.Interface as CC.UPI
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Slotting as CC.Slot
import qualified Cardano.Crypto as Crypto
import           Cardano.Prelude (panic)
import           Control.Monad.Except
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Coerce
import Data.FingerTree (Measured(..))
import Data.Word
import Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Crypto.DSIGN.Cardano
import           Ouroboros.Consensus.Crypto.DSIGN.Class (SignedDSIGN(..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT
import Ouroboros.Network.Block

-- | Hard-coded number of slots per epoch in the Byron era
byronEpochSlots :: CC.Slot.EpochSlots
byronEpochSlots = CC.Slot.EpochSlots 21600

-- | Newtype wrapper to avoid orphan instances
newtype ByronBlock = ByronBlock { unByronBlock :: CC.Block.ABlock ByteString }
  deriving (Eq, Show)

instance StandardHash ByronBlock

instance Measured BlockMeasure ByronBlock where
  measure = blockMeasure

instance HasHeader ByronBlock where
  type HeaderHash ByronBlock = CC.Block.HeaderHash

  blockHash = CC.Block.blockHashAnnotated . unByronBlock
  -- TODO distinguish the genesis hash? How do we do this after the fact?
  blockPrevHash = BlockHash . CC.Block.blockPrevHash . unByronBlock
  blockSlot = fromIntegral @Word64 . coerce . CC.Block.blockSlot . unByronBlock
  blockNo = BlockNo . CC.Common.unChainDifficulty . CC.Block.blockDifficulty . unByronBlock
  blockInvariant = const True


instance UpdateLedger ByronBlock where
  newtype LedgerState ByronBlock = ByronLedgerState CC.Block.ChainValidationState
    deriving (Eq, Show)
  newtype LedgerError ByronBlock = ByronLedgerError CC.Block.ChainValidationError
    deriving (Eq, Show)
  newtype LedgerConfig ByronBlock = ByronLedgerConfig Genesis.Config

  applyLedgerBlock (ByronLedgerConfig cfg) (ByronBlock block) (ByronLedgerState state)
    = mapExcept (bimap ByronLedgerError ByronLedgerState) $ do
      CC.Block.BodyState { CC.Block.utxo, CC.Block.updateState, CC.Block.delegationState } <- CC.Block.updateBody bodyEnv bodyState block
      pure $ state
        { CC.Block.cvsLastSlot     = CC.Block.blockSlot block
        , CC.Block.cvsPreviousHash = Right $ CC.Block.blockHashAnnotated block
        , CC.Block.cvsUtxo         = utxo
        , CC.Block.cvsUpdateState  = updateState
        , CC.Block.cvsDelegationState = delegationState
        }
    where
      bodyState = CC.Block.BodyState
        { CC.Block.utxo        = CC.Block.cvsUtxo state
        , CC.Block.updateState = CC.Block.cvsUpdateState state
        , CC.Block.delegationState = CC.Block.cvsDelegationState state
        }
      bodyEnv = CC.Block.BodyEnvironment
        { CC.Block.protocolMagic = Genesis.configProtocolMagic cfg
        , CC.Block.k = Genesis.configK cfg
        , CC.Block.numGenKeys
        , CC.Block.protocolParameters = CC.UPI.adoptedProtocolParameters . CC.Block.cvsUpdateState $ state
        , CC.Block.currentEpoch = CC.Slot.slotNumberEpoch (Genesis.configEpochSlots cfg) (CC.Block.blockSlot block)
        }
      numGenKeys :: Word8
      numGenKeys =
        case length (Genesis.unGenesisWStakeholders $ Genesis.configBootStakeholders cfg) of
          n
            | n > fromIntegral (maxBound :: Word8) -> panic
              "updateBody: Too many genesis keys"
            | otherwise -> fromIntegral n

  applyLedgerHeader (ByronLedgerConfig cfg) (ByronBlock block) (ByronLedgerState state)
    = mapExcept (bimap ByronLedgerError ByronLedgerState) $ do
      updateState <- CC.Block.updateHeader headerEnv (CC.Block.cvsUpdateState state) (CC.Block.blockHeader block)
      pure $ state
        { CC.Block.cvsLastSlot     = CC.Block.blockSlot block
        , CC.Block.cvsPreviousHash = Right $ CC.Block.blockHashAnnotated block
        , CC.Block.cvsUpdateState  = updateState
        }
    where
      headerEnv = CC.Block.HeaderEnvironment
        { CC.Block.protocolMagic = Genesis.configProtocolMagicId cfg
        , CC.Block.k          = Genesis.configK cfg
        , CC.Block.numGenKeys
        , CC.Block.delegationMap
        , CC.Block.lastSlot   = CC.Block.cvsLastSlot state
        }
      numGenKeys :: Word8
      numGenKeys =
        case length (Genesis.unGenesisWStakeholders $ Genesis.configBootStakeholders cfg) of
          n
            | n > fromIntegral (maxBound :: Word8) -> panic
              "updateBody: Too many genesis keys"
            | otherwise -> fromIntegral n

      delegationMap =
        CC.Delegation.asDelegationMap . CC.Delegation.isActivationState
        $ CC.Block.cvsDelegationState state

{-------------------------------------------------------------------------------
  Support for PBFT consensus algorithm
-------------------------------------------------------------------------------}

type instance BlockProtocol ByronBlock = PBft PBftCardanoCrypto

instance HasPreHeader ByronBlock where
  type PreHeader ByronBlock = CC.Block.ToSign
  blockPreHeader = unAnnotated . CC.Block.recoverSignedBytes byronEpochSlots
                   . CC.Block.blockHeader . unByronBlock

instance HasPayload (PBft PBftCardanoCrypto) ByronBlock where
  blockPayload _ (ByronBlock aBlock) = PBftPayload
    { pbftIssuer = VerKeyCardanoDSIGN
                   . Crypto.pskIssuerPk
                   . Crypto.psigPsk
                   . CC.Block.unBlockSignature
                   . CC.Block.consensusSignature
                   . CC.Block.headerConsensusData
                   . CC.Block.blockHeader
                   $ aBlock
    , pbftSignature = SignedDSIGN
                      . SigCardanoDSIGN
                      . Crypto.Signature
                      . Crypto.psigSig
                      . CC.Block.unBlockSignature
                      . CC.Block.consensusSignature
                      . CC.Block.headerConsensusData
                      . CC.Block.blockHeader
                      $ aBlock
    }

instance ProtocolLedgerView ByronBlock where
  protocolLedgerView _ns (ByronLedgerState ls) = PBftLedgerView
    -- Delegation map
    ( CC.Delegation.asDelegationMap
      . CC.Delegation.isActivationState
      . CC.Block.cvsDelegationState
      $ ls
    )

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

module Ouroboros.Storage.ChainDB.Impl (
    -- * Initializatio
    ChainDbArgs(..)
  , defaultArgs
  , openDB
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Data.Set (Set)
import qualified Data.Set as Set

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as Fragment
import           Ouroboros.Network.Block (ChainHash (..), HasHeader (..), Point,
                     StandardHash)
import qualified Ouroboros.Network.Block as Block

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Storage.ChainDB.API
import           Ouroboros.Storage.Common
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)

import           Ouroboros.Storage.ChainDB.ImmDB (ImmDB)
import qualified Ouroboros.Storage.ChainDB.ImmDB as ImmDB
import           Ouroboros.Storage.ChainDB.LgrDB (LgrDB)
import qualified Ouroboros.Storage.ChainDB.LgrDB as LgrDB
import           Ouroboros.Storage.ChainDB.VolDB (VolDB)
import qualified Ouroboros.Storage.ChainDB.VolDB as VolDB

{-------------------------------------------------------------------------------
  Arguments
-------------------------------------------------------------------------------}

data ChainDbArgs m blk hdr = forall h1 h2 h3. ChainDbArgs {

      -- Encoders and decoders

      cdbDecodeHash       :: forall s. Decoder s (HeaderHash blk)
    , cdbDecodeBlock      :: forall s. Decoder s blk
    , cdbDecodeLedger     :: forall s. Decoder s (LedgerState blk)
    , cdbDecodeChainState :: forall s. Decoder s (ChainState (BlockProtocol blk))
    , cdbEncodeHash       :: HeaderHash blk -> Encoding

      -- Error handling

    , cdbErrImmDb         :: ErrorHandling ImmDB.ImmutableDBError m
    , cdbErrVolDb         :: ErrorHandling (VolDB.VolatileDBError (HeaderHash blk)) m

      -- HasFS instances

    , cdbHasFSImmDb       :: HasFS m h1
    , cdbHasFSVolDb       :: HasFS m h2
    , cdbHasFSLgrDB       :: HasFS m h3

      -- Policy

    , cdbValidation       :: ImmDB.ValidationPolicy
    , cdbBlocksPerFile    :: Int
    , cdbMemPolicy        :: LgrDB.MemPolicy

      -- Integration

    , cdbNodeConfig       :: NodeConfig (BlockProtocol blk)
    , cdbEpochSize        :: EpochNo -> m EpochSize
    , cdbIsEBB            :: blk -> Maybe (HeaderHash blk)
    , cdbGetHeader        :: blk -> hdr
    , cdbGenesis          :: m (ExtLedgerState blk)
    }

-- | Default arguments for use within IO
--
-- See 'ImmDB.defaultArgs' and 'VolDB.defaultArgs' for a list of which fields
-- are not given a default and must therefore be set explicitly.
defaultArgs :: StandardHash blk => FilePath -> ChainDbArgs IO blk hdr
defaultArgs fp = toChainDbArgs ( ImmDB.defaultArgs fp
                               , VolDB.defaultArgs fp
                               , LgrDB.defaultArgs fp
                               )

-- | Internal: split chain DB args into imm DB and vol DB args
fromChainDbArgs :: ChainDbArgs m blk hdr
                -> ( ImmDB.ImmDbArgs m blk
                   , VolDB.VolDbArgs m blk hdr
                   , LgrDB.LgrDbArgs m blk
                   )
fromChainDbArgs ChainDbArgs{..} = (
      ImmDB.ImmDbArgs {
          immDecodeHash       = cdbDecodeHash
        , immDecodeBlock      = cdbDecodeBlock
        , immEncodeHash       = cdbEncodeHash
        , immErr              = cdbErrImmDb
        , immEpochSize        = cdbEpochSize
        , immValidation       = cdbValidation
        , immIsEBB            = cdbIsEBB
        , immHasFS            = cdbHasFSImmDb
        }
    , VolDB.VolDbArgs {
          volHasFS            = cdbHasFSVolDb
        , volErr              = cdbErrVolDb
        , volBlocksPerFile    = cdbBlocksPerFile
        , volDecodeBlock      = cdbDecodeBlock
        , volGetHeader        = cdbGetHeader
        }
    , LgrDB.LgrDbArgs {
          lgrNodeConfig       = cdbNodeConfig
        , lgrHasFS            = cdbHasFSLgrDB
        , lgrDecodeLedger     = cdbDecodeLedger
        , lgrDecodeChainState = cdbDecodeChainState
        , lgrDecodeHash       = cdbDecodeHash
        , lgrMemPolicy        = cdbMemPolicy
        , lgrGenesis          = cdbGenesis
        }
    )

-- | Internal: construct chain DB args from imm DB and vol DB
--
-- Useful in 'defaultArgs'
toChainDbArgs :: ( ImmDB.ImmDbArgs m blk
                 , VolDB.VolDbArgs m blk hdr
                 , LgrDB.LgrDbArgs m blk
                 )
              -> ChainDbArgs m blk hdr
toChainDbArgs ( ImmDB.ImmDbArgs{..}
              , VolDB.VolDbArgs{..}
              , LgrDB.LgrDbArgs{..}
              ) = ChainDbArgs{
      -- Encoders and decoders
      cdbDecodeHash       = immDecodeHash
    , cdbDecodeBlock      = immDecodeBlock
    , cdbDecodeLedger     = lgrDecodeLedger
    , cdbDecodeChainState = lgrDecodeChainState
    , cdbEncodeHash       = immEncodeHash
      -- Error handling
    , cdbErrImmDb         = immErr
    , cdbErrVolDb         = volErr
      -- HasFS instances
    , cdbHasFSImmDb       = immHasFS
    , cdbHasFSVolDb       = volHasFS
    , cdbHasFSLgrDB       = lgrHasFS
      -- Policy
    , cdbValidation       = immValidation
    , cdbBlocksPerFile    = volBlocksPerFile
    , cdbMemPolicy        = lgrMemPolicy
      -- Integration
    , cdbNodeConfig       = lgrNodeConfig
    , cdbEpochSize        = immEpochSize
    , cdbIsEBB            = immIsEBB
    , cdbGetHeader        = volGetHeader
    , cdbGenesis          = lgrGenesis
    }

{-------------------------------------------------------------------------------
  Internal environment
-------------------------------------------------------------------------------}

data ChainDbEnv m blk hdr = CDB {
      cdbImmDB   :: ImmDB m blk
    , cdbVolDB   :: VolDB m blk hdr
    , cdbLgrDB   :: LgrDB m blk
    , cdbChain   :: TVar m (AnchoredFragment hdr)
    , cdbInvalid :: TVar m (Set (Point blk))
    , cdbHeader  :: blk -> hdr
    }

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

openDB :: forall m blk hdr.
          ( MonadSTM   m
          , MonadST    m
          , MonadCatch m
          , HasHeader hdr
          , HeaderHash hdr ~ HeaderHash blk
          , ProtocolLedgerView blk
          )
       => ChainDbArgs m blk hdr -> m (ChainDB m blk hdr)
openDB args = do
    immDB   <- ImmDB.openDB argsImmDb
    volDB   <- VolDB.openDB argsVolDb
    lgrDB   <- LgrDB.openDB argsLgrDb
                            immDB
                            (implGetKnownBlock immDB volDB)
    chain   <- atomically $ newTVar undefined
    invalid <- atomically $ newTVar Set.empty
    let env = CDB { cdbImmDB   = immDB
                  , cdbVolDB   = volDB
                  , cdbLgrDB   = lgrDB
                  , cdbChain   = chain
                  , cdbInvalid = invalid
                  , cdbHeader  = cdbGetHeader args
                  }
    return ChainDB {
        addBlock           = undefined
      , getCurrentChain    = cdbGetCurrentChain    env
      , getCurrentLedger   = undefined
      , getTipBlock        = cdbGetTipBlock        env
      , getTipHeader       = cdbGetTipHeader       env
      , getTipPoint        = cdbGetTipPoint        env
      , getBlock           = cdbGetBlock           env
      , getIsFetched       = cdbGetIsFetched       env
      , streamBlocks       = cdbStreamBlocks       env
      , newReader          = undefined
      , knownInvalidBlocks = cdbKnownInvalidBlocks env
      , pointOnChain       = undefined
      }
  where
    (argsImmDb, argsVolDb, argsLgrDb) = fromChainDbArgs args

{-------------------------------------------------------------------------------
  Implementation
-------------------------------------------------------------------------------}

cdbGetIsFetched :: forall m blk hdr. MonadSTM m
                => ChainDbEnv m blk hdr
                -> STM m (Point blk -> Bool)
cdbGetIsFetched CDB{..} = basedOnHash <$> VolDB.getIsMember cdbVolDB
  where
    -- The volatile DB indexes by hash only, not by points. However, it should
    -- not be possible to have two points with the same hash but different
    -- slot numbers.
    basedOnHash :: (HeaderHash blk -> Bool) -> Point blk -> Bool
    basedOnHash f p =
        case Block.pointHash p of
          BlockHash hash -> f hash
          GenesisHash    -> False

cdbGetCurrentChain :: MonadSTM m
                   => ChainDbEnv m blk hdr
                   -> STM m (AnchoredFragment hdr)
cdbGetCurrentChain CDB{..} = readTVar cdbChain

cdbGetTipPoint :: ( MonadSTM m
                  , HasHeader hdr
                  , HeaderHash hdr ~ HeaderHash blk
                  )
               => ChainDbEnv m blk hdr
               -> STM m (Point blk)
cdbGetTipPoint = fmap (Block.castPoint . Fragment.headPoint)
               . cdbGetCurrentChain

cdbGetTipBlock :: ChainDbEnv m blk hdr
               -> m (Maybe blk)
cdbGetTipBlock = undefined

cdbGetTipHeader :: Monad m
                => ChainDbEnv m blk hdr
                -> m (Maybe hdr)
cdbGetTipHeader cdb@CDB{..} = fmap cdbHeader <$> cdbGetTipBlock cdb

cdbKnownInvalidBlocks :: MonadSTM m
                      => ChainDbEnv m blk hdr
                      -> STM m (Set (Point blk))
cdbKnownInvalidBlocks CDB{..} = readTVar cdbInvalid

cdbGetBlock :: ChainDbEnv m blk hdr
            -> Point blk -> m (Maybe blk)
cdbGetBlock CDB{..} = implGetBlock cdbImmDB cdbVolDB

cdbStreamBlocks :: ChainDbEnv m blk hdr
                -> StreamFrom blk -> StreamTo blk -> m (Iterator m blk)
cdbStreamBlocks CDB{..} = implStreamBlocks cdbImmDB cdbVolDB

{-------------------------------------------------------------------------------
  Lower level functionality

  These are functions that don't require all parts of the ChainDB to have
  been initialized
-------------------------------------------------------------------------------}

implGetBlock :: ImmDB m blk
             -> VolDB m blk hdr
             -> Point blk
             -> m (Maybe blk)
implGetBlock immDB volDB = undefined

implStreamBlocks :: ImmDB m blk
                 -> VolDB m blk hdr
                 -> StreamFrom blk -> StreamTo blk -> m (Iterator m blk)
implStreamBlocks = undefined

-- | Wrapper around 'implGetBlock' for blocks that we know should exist
--
-- If the block does /not/ exist, this is an indication of disk failure and
-- should trigger recovery.
implGetKnownBlock :: ImmDB m blk
                  -> VolDB m blk hdr
                  -> Point blk
                  -> m blk
implGetKnownBlock = undefined

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Topology where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString as B
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.String.Conv (toS)

import           Ouroboros.Consensus.Node (NodeId(..))

-- | A data structure bundling together a node identifier and the path to
-- the topology file.
data TopologyInfo = TopologyInfo {
    node         :: NodeId
  , topologyFile :: FilePath
  }

data NodeSetup = NodeSetup {
    nodeId    :: NodeId
  , producers :: [NodeId]
  , consumers :: [NodeId]
  }

instance FromJSON NodeId where
    parseJSON v = CoreId <$> parseJSON v

deriveFromJSON defaultOptions ''NodeSetup

data NetworkTopology = NetworkTopology [NodeSetup]

deriveFromJSON defaultOptions ''NetworkTopology

type NetworkMap = Map NodeId NodeSetup

toNetworkMap :: NetworkTopology -> NetworkMap
toNetworkMap (NetworkTopology xs) =
    foldl' (\acc ns -> M.insert (nodeId ns) ns acc) mempty xs

readTopologyFile :: FilePath -> IO (Either String NetworkTopology)
readTopologyFile topo = do
    eitherDecode . toS <$> B.readFile topo

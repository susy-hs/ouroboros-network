{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
module Ouroboros.Network.AnchoredFragment (
  -- * AnchoredFragment type and fundamental operations
  AnchoredFragment(Empty, (:>)),
  anchorPoint,
  unanchorFragment,
  mkAnchoredFragment,
  valid,
  validExtension,
  viewLeft,

  -- ** Block re-exports
  HasHeader(..),
  Point(..),
  castPoint,
  blockPoint,

  -- * AnchoredFragment construction and inspection
  -- ** Head inspection
  headPoint,
  headSlot,
  headHash,
  headBlockNo,

  -- ** Basic operations
  head,
  last,
  lastPoint,
  lastSlot,
  toNewestFirst,
  toOldestFirst,
  fromNewestFirst,
  fromOldestFirst,
  dropNewest,
  takeOldest,
  dropWhileNewest,
  length,
  null,

  -- ** Update type and operations
  ChainUpdate(..),
  addBlock,
  rollback,
  applyChainUpdate,
  applyChainUpdates,

  -- * Special operations
  pointOnFragment,
  withinFragmentBounds,
  findFirstPoint,
  successorBlock,
  selectPoints,
  isPrefixOf,
  splitAfterPoint,
  join,
  intersect,
  intersectionPoint,
  mapAnchoredFragment,

  -- * Conversion to/from Chain
  fromChain,
  toChain,
  anchorNewest,

  -- * Helper functions
  prettyPrint
  ) where

import           Prelude hiding (head, last, length, null)

import           Codec.CBOR.Encoding (Encoding)
import           Control.Exception (assert)
import           Data.List (find)
import           Data.Word (Word64)

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain)
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.ChainFragment (ChainFragment)
import qualified Ouroboros.Network.ChainFragment as CF


-- | An 'AnchoredFragment' is a 'ChainFragment' that is anchored to some
-- 'Point': the point right before the first, leftmost block in the fragment.
-- The anchor point can be thought of as a left exclusive bound.
--
-- For example, the following fragment is anchored at @a@ and contains @b1@,
-- @b2@, and @b3@, which is the head of the fragment.
--
-- > a ] b1 >: b2 >: b3
--
-- The fact that it is an /exclusive/ bound is particularly convenient when
-- dealing with Genesis. Genesis is the start of the chain, but not an actual
-- block, so we cannot use it an inclusive bound. However, there /is/ a
-- 'Point' that refers to Genesis ('Chain.genesisPoint'), which can be used as
-- the anchor point, acting as an exclusive bound.
--
-- An 'AnchoredFragment' anchored at Genesis, can thus be converted to a
-- 'Ouroboros.Network.Chain.Chain' ('toChain'), containing all blocks starting
-- from Genesis.
--
-- Without an anchor point, an empty 'ChainFragment' doesn't give us much more
-- information: is it empty because the whole chain is empty? Or, did we just
-- get an empty fragment that was split off from some later part of the chain?
--
-- Furthermore, an important criterion during chain selection is the length of
-- the chain. However, comparing the length of two 'ChainFragment's doesn't
-- make much sense, since they might start at different points. Hence the idea
-- to \"anchor\" fragments at some point, and require that fragments have the
-- same anchor when comparing their lengths.
--
-- Note: instead of comparing the lengths of two 'ChainFragment's, we could
-- try to compare the 'blockNo' of their heads, which is also a measure of the
-- total length of the chain. However, EBBs throw a(nother) spanner in the
-- works: each EBB shares its 'blockNo' with a regular block, so comparing a
-- 'ChainFragment' that only contains the EBB with a 'ChainFragment' that only
-- contains the regular block with the same 'blockNo' as the EBB, will not
-- give a conclusive preference to either fragment, while in reality one
-- fragment actually corresponds to a longer chain.
data AnchoredFragment block = AnchoredFragment
    { anchorPoint      :: !(Point block)
    , unanchorFragment :: !(ChainFragment block)
    } deriving (Show, Eq)

mkAnchoredFragment :: HasHeader block
                   => (block -> Encoding)
                   -> Point block -> ChainFragment block
                   -> AnchoredFragment block
mkAnchoredFragment toEnc a c = case CF.last c of
    Nothing -> AnchoredFragment a CF.Empty
    Just b  -> assert (validExtension toEnc (Empty a) b) $
               AnchoredFragment a c

-- | \( O(1) \). Pattern for matching on or creating an empty
-- 'AnchoredFragment'. An empty fragment has/needs an anchor point.
pattern Empty :: HasHeader block => Point block -> AnchoredFragment block
pattern Empty a <- (viewRight -> EmptyR a)
  where
    Empty a = AnchoredFragment a CF.Empty

-- | Auxiliary data type to define the pattern synonym
data ViewRight block
    = EmptyR (Point block)
    | ConsR  (AnchoredFragment block) block

viewRight :: HasHeader block => AnchoredFragment block -> ViewRight block
viewRight (AnchoredFragment a c) = case c of
    CF.Empty   -> EmptyR a
    c' CF.:> b -> ConsR (AnchoredFragment a c') b

-- | \( O(1) \). Add a block to the right of the anchored fragment.
pattern (:>) :: HasHeader block
             => AnchoredFragment block -> block -> AnchoredFragment block
pattern af' :> b <- (viewRight -> ConsR af' b)
  where
    af@(AnchoredFragment a c) :> b = AnchoredFragment a (c CF.:> b)

-- | Auxiliary data type to define the pattern synonym
data ViewLeft block
    = EmptyL (Point block)
    | ConsL  block (AnchoredFragment block)

-- | \( O(1) \). View the first, leftmost block of the anchored fragment.
viewLeft :: HasHeader block => (block -> Encoding) -> AnchoredFragment block -> ViewLeft block
viewLeft toEnc (AnchoredFragment a c) = case c of
    CF.Empty   -> EmptyL a
    b CF.:< c' -> ConsL b (AnchoredFragment (blockPoint toEnc b) c')

infixl 5 :>

{-# COMPLETE Empty, (:>) #-}

prettyPrint :: HasHeader block
            => String
            -> (Point block -> String)
            -> (block -> String)
            -> AnchoredFragment block
            -> String
prettyPrint nl ppAnchor ppBlock (AnchoredFragment a c) =
    CF.foldChainFragment (\s b -> s ++ nl ++ "    " ++ ppBlock b)
    ("AnchoredFragment (" <> ppAnchor a <> "):") c


-- | \( O(n) \).
valid :: HasHeader block => (block -> Encoding) -> AnchoredFragment block -> Bool
valid _ (Empty _) = True
valid toEnc (af :> b) = valid toEnc af && validExtension toEnc af b

-- | \( O(1) \).
validExtension :: HasHeader block => (block -> Encoding) -> AnchoredFragment block -> block -> Bool
validExtension toEnc af bSucc =
    blockInvariant bSucc &&
    case head af of
      Left  p -> pointHash p == blockPrevHash bSucc &&
                 pointSlot p <  blockSlot     bSucc
      Right b -> CF.isValidSuccessorOf toEnc bSucc b

-- | \( O(1) \). When the fragment is empty, return the anchor point,
-- otherwise the most recently added block.
head :: HasHeader block => AnchoredFragment block -> Either (Point block) block
head (_ :> b)  = Right b
head (Empty a) = Left a

-- | \( O(1) \). When the fragment is empty, the anchor point is returned.
headPoint :: HasHeader block => (block -> Encoding) -> AnchoredFragment block -> Point block
headPoint toEnc = either id (blockPoint toEnc) . head

-- | \( O(1) \). When the fragment is empty, the slot of the anchor point is
-- returned.
headSlot :: HasHeader block => AnchoredFragment block -> SlotNo
headSlot = either pointSlot blockSlot . head

-- | \( O(1) \). When the fragment is empty, the hash of the anchor point is
-- returned.
headHash :: HasHeader block => (block -> Encoding) -> AnchoredFragment block -> ChainHash block
headHash toEnc = either pointHash (BlockHash . blockHash toEnc) . head

-- | \( O(1) \). When the fragment is empty, 'Nothing' is returned, as the
-- anchor point has no 'BlockNo'.
headBlockNo :: HasHeader block => AnchoredFragment block -> Maybe BlockNo
headBlockNo = either (const Nothing) (Just . blockNo) . head

-- | \( O(1) \). When the fragment is empty, return the anchor point,
-- otherwise the leftmost block.
last :: HasHeader block => (block -> Encoding) -> AnchoredFragment block -> Either (Point block) block
last toEnc (viewLeft toEnc -> ConsL b _)  = Right b
last _ (Empty a) = Left a

-- | \( O(1) \). When the fragment is empty, the anchor point is returned.
lastPoint :: HasHeader block => (block -> Encoding) -> AnchoredFragment block -> Point block
lastPoint toEnc = either id (blockPoint toEnc) . last toEnc

-- | \( O(1) \). When the fragment is empty, the slot of the anchor point is
-- returned.
lastSlot :: HasHeader block => (block -> Encoding) -> AnchoredFragment block -> SlotNo
lastSlot toEnc = either pointSlot blockSlot . last toEnc

-- | TODO. Make a list of blocks from a 'AnchoredFragment', in newest-to-oldest
-- order.
toNewestFirst :: HasHeader block => AnchoredFragment block -> [block]
toNewestFirst = CF.toNewestFirst . unanchorFragment

-- | \( O(n) \). Make a list of blocks from a 'AnchoredFragment', in
-- oldest-to-newest order.
toOldestFirst :: HasHeader block => AnchoredFragment block -> [block]
toOldestFirst = CF.toOldestFirst . unanchorFragment

-- | \( O(n) \). Make a 'AnchoredFragment' from a list of blocks in
-- newest-to-oldest order. The last block in the list must be the block
-- following the given anchor point.
fromNewestFirst :: HasHeader block
                => Point block  -- ^ Anchor
                -> [block] -> AnchoredFragment block
fromNewestFirst a = foldr (flip (:>)) (Empty a)

-- | \( O(n) \). Make a 'AnchoredFragment' from a list of blocks in
-- oldest-to-newest order. The first block in the list must be the block
-- following the given anchor point.
fromOldestFirst :: HasHeader block
                => (block -> Encoding)
                -> Point block  -- ^ Anchor
                -> [block] -> AnchoredFragment block
fromOldestFirst toEnc a bs = mkAnchoredFragment toEnc a (CF.fromOldestFirst toEnc bs)

-- | \( O(\log(\min(i,n-i)) \). Drop the newest @n@ blocks from the
-- 'AnchoredFragment'. The anchor point is not changed.
dropNewest :: HasHeader block
           => Int  -- ^ @n@
           -> AnchoredFragment block -> AnchoredFragment block
dropNewest n (AnchoredFragment a c) = AnchoredFragment a $ CF.dropNewest n c

-- | \( O(\log(\min(i,n-i)) \). Take the oldest @n@ blocks from the
-- 'AnchoredFragment'. The anchor point is not changed.
takeOldest :: HasHeader block
           => Int  -- ^ @n@
           -> AnchoredFragment block -> AnchoredFragment block
takeOldest n (AnchoredFragment a c) = AnchoredFragment a $ CF.takeOldest n c

-- | \( O(n) \). Drop the newest blocks that satisfy the predicate, keeping
-- the remainder. The anchor point is not changed.
dropWhileNewest :: HasHeader block
                => (block -> Bool)
                -> AnchoredFragment block
                -> AnchoredFragment block
dropWhileNewest p (AnchoredFragment a c) =
    AnchoredFragment a (CF.dropWhileNewest p c)

-- | \( O(1) \). Return the number of blocks. The anchor point is not counted.
length :: HasHeader block => AnchoredFragment block -> Int
length = CF.length . unanchorFragment

-- | \( O(1) \). The anchor point is not counted.
null :: AnchoredFragment block -> Bool
null = CF.null . unanchorFragment

-- | \( O(1) \). Add a block to the right of the anchored fragment.
--
-- Synonym for ':>'.
addBlock :: HasHeader block
         => block -> AnchoredFragment block -> AnchoredFragment block
addBlock b c = c :> b

-- | \( O(\log(\min(i,n-i)) \). If the 'Point' is within the bounds of the
-- 'AnchoredFragment' (see 'withinFragmentBounds'), roll back the anchored
-- fragment such that its head is the given point. In case the given point was
-- the anchor point, the returned anchored fragment will be empty.
--
-- In other words, remove blocks from the end of the 'AnchoredFragment' until
-- the given 'Point' is the head. If the given 'Point' is not within the
-- bounds of the 'AnchoredFragment', return 'Nothing'.
rollback :: HasHeader block
         => (block -> Encoding)
         -> Point block -> AnchoredFragment block
         -> Maybe (AnchoredFragment block)
rollback toEnc p (AnchoredFragment a c)
    | p == a
    = Just (Empty a)
    | otherwise
    = (AnchoredFragment a) <$> CF.rollback toEnc p c

-- | \( O(o \log(\min(i,n-i))) \). See 'CF.selectPoints'.
--
-- The list of offsets must be increasing monotonically.
--
-- __Note__: offset @n@, where @n@ equals the length of the anchored fragment,
-- corresponds to the anchor point. When the fragment is empty, offset 0 will
-- thus correspond to the anchor point.
selectPoints :: HasHeader block
             => (block -> Encoding) -> [Int] -> AnchoredFragment block -> [Point block]
selectPoints toEnc offsets (AnchoredFragment a c) =
    CF.selectPoints toEnc offsetsOnFrag c <> map (const a) anchorOffsets
  where
    len = CF.length c
    (offsetsOnFrag, offsetsAfterFrag) = span (< len) offsets
    anchorOffsets = takeWhile (== len) offsetsAfterFrag

-- | \( O(\log(\min(i,n-i)) \). Find the block after the given point. If the
-- given point is the anchor point, then the first block is returned (if there
-- is one).
successorBlock :: HasHeader block
               => (block -> Encoding) -> Point block -> AnchoredFragment block -> Maybe block
successorBlock toEnc p af@(AnchoredFragment a c)
    | p == a
    = either (const Nothing) Just $ last toEnc af
    | otherwise
    = CF.successorBlock toEnc p c

-- | \( O(\log(\min(i,n-i)) \). Same as 'CF.pointOnChainFragment': does the
-- fragment contain a block with the given block? The anchor point is ignored.
pointOnFragment :: HasHeader block
                => (block -> Encoding) -> Point block -> AnchoredFragment block -> Bool
pointOnFragment toEnc p (AnchoredFragment _ c) = CF.pointOnChainFragment toEnc p c

-- | \( O(\log(\min(i,n-i)) \). Is the point within the fragment bounds?
-- Either the point is the anchor point, or it corresponds to a block \"on\"
-- the fragment.
withinFragmentBounds :: HasHeader block
                     => (block -> Encoding)
                     -> Point block
                     -> AnchoredFragment block
                     -> Bool
withinFragmentBounds toEnc p (AnchoredFragment a c) =
    p == a || CF.pointOnChainFragment toEnc p c


-- | \( O(p \log(\min(i,n-i)) \). Find the first 'Point' in the list of points
-- that is within the fragment bounds. Return 'Nothing' if none of them are.
--
-- __Note__: in contrast to 'CF.findFirstPoint', this is based on
-- 'withinFragmentBounds' instead of 'CF.pointOnFragment'.
findFirstPoint
  :: HasHeader block
  => (block -> Encoding)
  -> [Point block]
  -> AnchoredFragment block
  -> Maybe (Point block)
findFirstPoint toEnc ps c = find (\b -> withinFragmentBounds toEnc b c) ps


applyChainUpdate :: HasHeader block
                 => (block -> Encoding)
                 -> ChainUpdate block
                 -> AnchoredFragment block
                 -> Maybe (AnchoredFragment block)
applyChainUpdate _ (AddBlock b) c = Just (addBlock b c)
applyChainUpdate toEnc (RollBack p) c = rollback toEnc p c

applyChainUpdates :: HasHeader block
                  => (block -> Encoding)
                  -> [ChainUpdate block]
                  -> AnchoredFragment block
                  -> Maybe (AnchoredFragment block)
applyChainUpdates _ []     c = Just c
applyChainUpdates toEnc (u:us) c = applyChainUpdates toEnc us =<< applyChainUpdate toEnc u c

-- | Convert a 'Chain' to an 'AnchoredFragment'.
--
-- The anchor of the fragment will be 'Chain.genesisPoint'.
fromChain :: HasHeader block => Chain block -> AnchoredFragment block
fromChain = fromNewestFirst Chain.genesisPoint . Chain.toNewestFirst

-- | Convert an 'AnchoredFragment' to a 'Chain'.
--
-- The anchor of the fragment must be 'Chain.genesisPoint', otherwise
-- 'Nothing' is returned.
toChain :: HasHeader block => (block -> Encoding) -> AnchoredFragment block -> Maybe (Chain block)
toChain toEnc af@(AnchoredFragment a _)
    | a == Chain.genesisPoint
    = Just $ Chain.fromNewestFirst toEnc $ toNewestFirst af
    | otherwise
    = Nothing

-- | Take the @n@ newest blocks from the chain and turn them into an
-- 'AnchoredFragment'.
--
-- When the chain itself is shorter than @n@ blocks, the fragment will also be
-- shorter than @n@ blocks (and anchored at genesis).
anchorNewest :: forall block. HasHeader block
             => (block -> Encoding)
             -> Word64  -- ^ @n@
             -> Chain block
             -> AnchoredFragment block
anchorNewest toEnc = go CF.Empty
  where
    -- Walk back over the chain, building up a chain fragment until k = 0 or
    -- we encountered genesis, then anchor the built-up chain fragment
    go :: ChainFragment block -> Word64 -> Chain block -> AnchoredFragment block
    go cf _ Chain.Genesis   = mkAnchoredFragment toEnc Chain.genesisPoint cf
    go cf 0 (_  Chain.:> b) = mkAnchoredFragment toEnc (blockPoint toEnc b) cf
    go cf n (ch Chain.:> b) = go (b CF.:< cf) (n - 1) ch

-- | \( O(\max(n_1, n_2)) \). Check whether the first anchored fragment is a
-- prefix of the second.
--
-- The two 'AnchoredFragment's must have the same anchor point, otherwise the
-- first cannot be a prefix of the second.
isPrefixOf :: (HasHeader block, Eq block)
           => AnchoredFragment block -> AnchoredFragment block -> Bool
AnchoredFragment a1 c1 `isPrefixOf` AnchoredFragment a2 c2 =
    a1 == a2 && c1 `CF.isPrefixOf` c2

-- | \( O(\log(\min(i,n-i)) \). Split the 'AnchoredFragment' after the given
--  'Point'. Return 'Nothing' if given 'Point' is not within the fragment
--  bounds ('withinFragmentBounds').
--
-- The given 'Point' may be the anchor point of the fragment, in which case
-- the empty fragment with the given anchor point and the original fragment
-- are returned.
splitAfterPoint
   :: forall block1 block2.
      (HasHeader block1, HasHeader block2, HeaderHash block1 ~ HeaderHash block2)
   => (block1 -> Encoding)
   -> AnchoredFragment block1
   -> Point block2
   -> Maybe (AnchoredFragment block1, AnchoredFragment block1)
splitAfterPoint toEnc c pt = case CF.splitAfterPoint toEnc (unanchorFragment c) pt of
   Just (cp, cs)
     -> let p = mkAnchoredFragment toEnc (anchorPoint c) cp
        in Just (p, mkAnchoredFragment toEnc (headPoint toEnc p) cs)
   Nothing
     | anchorPoint c == castPoint pt
     -> Just (mkAnchoredFragment toEnc (anchorPoint c) CF.Empty, c)
     | otherwise
     -> Nothing

-- | \( O(\log(\min(n_1, n_2))) \). Join two anchored fragments if the anchor
-- of the second fragment is the head (newest block) of the first fragment.
--
-- If the first fragment is empty, it can be joined if its anchor is the same
-- as the second fragment's anchor.
--
-- The returned fragment will have the same anchor as the first fragment.
join :: HasHeader block
     => (block -> Encoding)
     -> AnchoredFragment block
     -> AnchoredFragment block
     -> Maybe (AnchoredFragment block)
join toEnc af1@(AnchoredFragment a1 c1) af2@(AnchoredFragment a2 c2) =
    case head af1 of
      -- First fragment is empty
      Left _
        | a1 == a2
        -> Just af2
        | otherwise
        -> Nothing
      Right b1Head
        | blockPoint toEnc b1Head == a2
        -> mkAnchoredFragment toEnc a1 <$> CF.joinChainFragments toEnc c1 c2
        | otherwise
        -> Nothing

-- | \( O(n_2 \log(n_1)) \). Look for the most recent intersection of two
-- 'AnchoredFragment's @c1@ and @c2@.
--
-- The fragments need not have the same anchor point.
--
-- If they intersect, i.e., share a common 'Point' (possibly the anchor
-- point), then return a tuple of:
--
-- * @p1@: the prefix of the first  fragment
-- * @p2@: the prefix of the second fragment
-- * @s1@: the suffix of the first  fragment
-- * @s2@: the suffix of the second fragment
--
-- @p1@ and @p2@ will have the same /head/ (possibly an anchor point), namely
-- the intersection point @i@. The original chain @c1@ can be obtained by
-- putting @s1@ after @p1@, similarly for @c2@: by putting @s2@ after @p2@:
--
-- @
-- Just c1 = 'join' p1 s1
-- Just c2 = 'join' p2 s2
-- @
--
-- Take for example the following two fragments that share blocks 4 and 5. The
-- two fragments are fragments of the same chain, but don't contain all blocks
-- of the original chain. The anchor points of the fragments are indicated
-- with an asterisk (*). The @-A@ and @-B@ suffixes denote that blocks are
-- part of a fork of the chain.
--
-- >
-- >
-- >     ┆ 1*┆
-- >     ├───┤
-- >     │ 2 │     ┆ 2*┆
-- >     ├───┤     ├───┤
-- >     │ 4 │     │ 4 │
-- >     ├───┤     ├───┤
-- >     │ 5 │     │ 5 │
-- > ────┼───┼─────┼───┼───
-- >     │ 6A│     │ 6B│
-- >     └───┘     ├───┤
-- >               │ 8B│
-- >               └───┘
-- >       c1        c2
--
-- The intersection of @c1@ and @c2@ is block 5 (the last 'Point' the two
-- fragments have in common) and we return the following fragments:
--
-- >
-- >
-- >     ┆ 1*┆
-- >     ├───┤
-- >     │ 2 │     ┆ 2*┆
-- >     ├───┤     ├───┤
-- >     │ 4 │     │ 4 │
-- >     ├───┤     ├───┤
-- >     │ 5 │     │ 5 │      ┆ 5*┆     ┆ 5*┆
-- > ────┴───┴─────┴───┴──────┼───┼─────┼───┼──
-- >                          │ 6A│     │ 6B│
-- >                          └───┘     ├───┤
-- >                                    │ 8B│
-- >                                    └───┘
-- > Just (p1,       p2,        s1,       s2)
--
-- The intersection point will be the anchor point of fragments @s1@ and @s2@.
-- Fragment @p1@ will have the same anchor as @c1@ and @p2@ will have the same
-- anchor as @c2@.
--
-- Note that an empty fragment can still intersect another fragment, as its
-- anchor point can still intersect the other fragment. In that case the
-- respective prefix and suffix are both equal to original empty fragment.
-- Additionally, two empty fragments intersect if their anchor points are
-- equal, in which case all prefixes and suffixes are equal to the empty
-- fragment with the anchor point in question.
intersect
    :: forall block1 block2.
       (HasHeader block1, HasHeader block2, HeaderHash block1 ~ HeaderHash block2)
    => (block1 -> Encoding)
    -> (block2 -> Encoding)
    -> AnchoredFragment block1
    -> AnchoredFragment block2
    -> Maybe (AnchoredFragment block1, AnchoredFragment block2,
              AnchoredFragment block1, AnchoredFragment block2)
intersect toEnc1 toEnc2 c1 c2 = go c2
  where
    go :: AnchoredFragment block2
       -> Maybe (AnchoredFragment block1, AnchoredFragment block2,
                 AnchoredFragment block1, AnchoredFragment block2)
    go (Empty a2)
      | Just (p1, s1) <- splitAfterPoint toEnc1 c1 a2
      = Just (p1, mkAnchoredFragment toEnc2 a2 CF.Empty, s1, c2)
      | otherwise
      = Nothing
    go (c2' :> b)
      | let pt = blockPoint toEnc2 b
      , Just (p1, s1) <- splitAfterPoint toEnc1 c1 pt
      , Just (p2, s2) <- splitAfterPoint toEnc2 c2 pt
        -- splitAfterPoint c2 pt cannot fail,
        -- since pt comes out of c2
      = Just (p1, p2, s1, s2)
      | otherwise
      = go c2'

-- | \( O(n_2 \log(n_1)) \). Look for the most recent intersection point of
-- two 'AnchoredFragment's
--
-- The fragments need not have the same anchor point.
--
-- Reusing the example in the docstring of 'intersect': this function will
-- return the anchor point @5*@.
intersectionPoint
    :: (HasHeader block1, HasHeader block2, HeaderHash block1 ~ HeaderHash block2)
    => (block1 -> Encoding)
    -> (block2 -> Encoding)
    -> AnchoredFragment block1
    -> AnchoredFragment block2
    -> Maybe (Point block1)
intersectionPoint toEnc1 toEnc2 c1 c2 = case intersect toEnc1 toEnc2 c1 c2 of
    Just (_, _, s1, _) -> Just (anchorPoint s1)
    Nothing            -> Nothing

-- | \( O(n) \). Maps over the chain blocks. This is not allowed to change the
-- block `Point`s, or it would create an invalid chain. The 'anchorPoint' is
-- not affected.
--
mapAnchoredFragment :: (HasHeader block1, HasHeader block2,
                        HeaderHash block1 ~ HeaderHash block2)
                 => (block1 -> block2)
                 -> AnchoredFragment block1
                 -> AnchoredFragment block2
mapAnchoredFragment f (AnchoredFragment a c) =
    AnchoredFragment (castPoint a) (CF.mapChainFragment f c)

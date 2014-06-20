{-
Simple random access list (fast prototyping on Haskell for C++ language).

  It is random access list (ral) based on simplest self-balancing binary
  search tree with implicit keys.
  Code is pure functional.
  Tested by unit test framework QuickCheck.
  Project writed as fast prototyping for C++ language
  and it have many disadvantages for Haskell:
  * Not splited for files.
  * Haven't function signatures.
  * Not full covered by unit tests.
  * Not use selfdocumented code style.
  * ral interface functions not suited for a haskell style (without zippers, without change position of container parameter to last and others).
  * ral interface functions have collisions with functions from Data.List module and others.
  * ral interface not exported as module.
  * ral not derived and not instanced many required by Haskell type classes, for example "Foldable", "Monoid" and many others.
  * Not corrected strictness and laziness.
  * Functions not written with tail recursion, not optimized memory consumption of tree.
  * Not configured git:ignore list.


Use indices as iterators and it is not stable despite of iterators on ::std::list<T>.
Average and worst case operation complexity and memory consumption is O(log n) for all operations with one element.
Average operation complexity of traverse tree to next element and get its value is O(1) (I mean, traverse without using iterators).

RAL on C++ loses pure functional realization and have much changed interface.
-}

import Data.Maybe
import Data.List (splitAt, foldl')
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Ord
import System.Random
import Data.Ix

type Height = Int
type Size = Int
data Tree a = None | Node (Tree a) (Tree a) a Height Size deriving (Show)

instance (Ord a) => Ord (Tree a) where --For fun
    t1 `compare` t2 = comparing toList t1 t2
instance (Eq a) => Eq (Tree a) where --For fun
    t1 == t2 = (toList t1) == (toList t2)

--interface:
height None = 0
height (Node _ _ _ h _) = h

size None = 0
size (Node _ _ _ _ s) = s

get None _ = Nothing
get _ i | i<0 = Nothing
get (Node l r v _ _) i
    | i > size l  = get r $ indexSkipLeft l i
    | i < size l  = get l i
    | otherwise   = Just v

change None _ _ = undefined
change _ i _ | i<0 = undefined
change (Node l r v h s) i nv
    | i > size l  = Node l (change r (indexSkipLeft l i) nv) v h s
    | i < size l  = Node (change l i nv) r v h s
    | otherwise   = Node l r nv h s

insert None 0 nv = Node None None nv 1 1
insert _ i _ | i<0 = undefined
insert None i _ | i>0 = undefined
insert (Node l r v _ s) i nv
    | i > size l    = let
        nr = insert r (indexSkipLeft l i) nv
        nh = max (height l) (height nr) + 1
        ns = s + 1 in check_and_fix_balance $ Node l nr v nh ns
    | i < size l    = let
        nl = insert l i nv
        nh = max (height nl) (height r) + 1
        ns = s + 1 in check_and_fix_balance $ Node nl r v nh ns
    | otherwise     = let
        nl = insertNearBefore l nv
        nh = max (height nl) (height r) + 1
        ns = s + 1 in check_and_fix_balance $ Node nl r v nh ns
    where
        insertNearBefore None nv =  Node None None nv 1 1
        insertNearBefore (Node l r v h s) nv = check_and_fix_balance $ Node l nr v nh ns where
            nr = insertNearBefore r nv
            nh = max (height l) (height nr) + 1
            ns = s + 1

delete None 0 = undefined
delete _ i | i<0 = undefined
delete None i | i>0 = undefined
delete (Node l r v _ s) i
    | i > size l    = let
        nr = delete r (indexSkipLeft l i)
        nh = max (height l) (height nr) + 1
        ns = s - 1 in check_and_fix_balance $ Node l nr v nh ns
    | i < size l    = let
        nl = delete l i
        nh = max (height nl) (height r) + 1
        ns = s - 1 in check_and_fix_balance $ Node nl r v nh ns
    | otherwise     = moveToTopFromRightmostAtLeftNodeOrAssignRight l r s
    where
        moveToTopFromRightmostAtLeftNodeOrAssignRight None r _ = r
        moveToTopFromRightmostAtLeftNodeOrAssignRight l r s = check_and_fix_balance $ Node lc r movedValue nh ns
            where
            (movedValue, lc) = moveToTopFromRightmost l
            ns = s - 1
            nh = height lc + height r + 1
        moveToTopFromRightmost None  = undefined -- reach only if delete algorithm have error
        moveToTopFromRightmost (Node l None v _ s) = (v, l)
        moveToTopFromRightmost (Node l r v _ s) = (movedValue, check_and_fix_balance $ Node l nr v nh ns)
            where
            (movedValue, nr) =  moveToTopFromRightmost r
            ns = s - 1
            nh = height l + height nr + 1

push_front t v = insert t (begin t) v
push_back t v = insert t (end t) v

pop_front None = undefined
pop_front t = delete t $ begin t

pop_back None = undefined
pop_back t = delete t $ end t - 1

front None = undefined
front t = get t $ begin t

back None = undefined
back t = get t $ end t - 1

add_range :: Tree a -> [a] -> Tree a
add_range t l = foldl' push_back t l

make_random_access_list l = add_range None l

toList None = []
toList (Node l r v _ _) = toList l ++ (v:toList r)  --At least this function should be rewritten with tail recursion,
-- because work with big data amount.

empty None = True
empty (Node _ _ _ _ _) = False

begin _ = 0
end t = size t

--internal:
unimplemented = undefined

lrotate None = undefined
lrotate (Node _ None _ _ _) = undefined
lrotate t@(Node rl rr@(Node pl pr pv ph ps) rv rh rs) = Node npl pr pv nph nps where
    nrs = size t - size rr + size pl
    nps = nrs + size pr + 1
    npl = Node rl pl rv nrh nrs
    nrh = max (height rl) (height pl) + 1
    nph = max nrh (height pr) + 1

rrotate None = undefined
rrotate (Node None _ _ _ _) = undefined
rrotate t@(Node rl@(Node pl pr pv ph ps) rr rv rh rs) = Node pl npr pv nph nps where
    nrs = size t - size rl + size pr
    nps = nrs + size pl + 1
    npr = Node pr rr rv nrh nrs
    nrh = max (height rr) (height pr) + 1
    nph = max (height pl) nrh + 1

check_and_fix_balance None = None
check_and_fix_balance t@(Node l r _ _ _)
    | height l - height r > max_tree_height_disbalance = rrotate t
    | height l - height r < -max_tree_height_disbalance = lrotate t --
    | otherwise = t

indexSkipLeft lc i = i - (size lc + 1)

max_tree_height_disbalance = 2

--tests:
should_be_equality_comparable l l2 =
    (l == l2) == ((make_random_access_list l) == (make_random_access_list l2))
    &&
    (not $ l == l2) == ((make_random_access_list l) /= (make_random_access_list l2))
    &&
    (let ral = make_random_access_list l in ral == ral)

should_be_ordering_comparable l l2 =
    (compare l l2) == (compare (make_random_access_list l) (make_random_access_list l2))
    &&
    (let ral = make_random_access_list l in EQ == (compare ral ral))

should_construct_RAL_from_list::[Int]->Bool
should_construct_RAL_from_list l = l == (toList $ make_random_access_list l)

should_get_item_by_index_when_exist l i =
    get (make_random_access_list l) i == getl l i

should_change_item_by_index::[Int]->Int->Property
should_change_item_by_index l i_ =
    (length l > i) ==>
        (toList (change (make_random_access_list l) i newValue)) == list_with_newValue_at_index
    where
        i = abs i_
        newValue = 777
        list_with_newValue_at_index =
            fst $ unzip $
                map (\r@(_,ii) -> if ii==i then (newValue, i) else r) $
                    zip l [0..]

should_back_addable l v =
        (toList (push_back (make_random_access_list l) v)) == (l ++ [v])

should_front_addable l v =
        (toList (push_front (make_random_access_list l) v)) == (v:l)

should_back_popable l =
    not (null l) ==>
        (toList (pop_front (make_random_access_list l))) == (tail l)

should_front_popable l =
    not (null l) ==>
        (toList (pop_back (make_random_access_list l))) == (init l)

should_allow_random_insertions l maxInsertionAmount seed =
    maxInsertionAmount >= 0 ==>
        afterInsertionsInList == (toList afterInsertionsInRAL) where
            initialSourceLength = length l
            ral = make_random_access_list l
            (insertionAmount, stdgen) = maxInsertionAmountWithStdgen maxInsertionAmount (mkStdGen seed)
            indicesForInsertion = take insertionAmount $ insertPositions initialSourceLength stdgen
            valuesForInsertion = randomRs (1000, 1200) stdgen ::[Int]
            insertionData = zip indicesForInsertion valuesForInsertion
            afterInsertionsInList = foldl (\l (i,v) -> insertToListByIndexIfCan l i v) l insertionData
            afterInsertionsInRAL = foldl (\t (i,v) -> insert t i v) ral insertionData

should_be_getable_after_random_insertions::[Int]->Int->Int->Int->Property
should_be_getable_after_random_insertions l maxInsertionAmount seed seed2 =
    maxInsertionAmount >= 0 ==>
        afterInsertionsInList == (toList afterInsertionsInRAL)
        &&
        (map (getl afterInsertionsInList) indicesForGet) == (map (get afterInsertionsInRAL) indicesForGet)
        where
            initialSourceLength = length l
            ral = make_random_access_list l
            (insertionAmount, stdgen) = maxInsertionAmountWithStdgen maxInsertionAmount (mkStdGen seed)
            indicesForInsertion = take insertionAmount $ insertPositions initialSourceLength stdgen
            indicesForGet = take (insertionAmount + initialSourceLength + 10) $
                randomRs (0, length afterInsertionsInList - 1) (mkStdGen seed2)
            valuesForInsertion = randomRs (1000, 1200) stdgen ::[Int]
            insertionData = zip indicesForInsertion valuesForInsertion
            afterInsertionsInList = foldl (\l (i,v) -> insertToListByIndexIfCan l i v) l insertionData
            afterInsertionsInRAL = foldl (\t (i,v) -> insert t i v) ral insertionData

should_allow_random_deletions l maxDeletionAmount seed =
    maxDeletionAmount >= 0 && not (null l) ==>
        afterDeletionsInList == (toList afterDeletionsInRAL) where
            initialSourceLength = length l
            ral = make_random_access_list l
            (deletionAmount, stdgen) = maxDeletionAmountWithStdgen maxDeletionAmount initialSourceLength (mkStdGen seed)
            indicesForDeletion = take deletionAmount $ deletePositions initialSourceLength stdgen
            afterDeletionsInList = foldl (\l i -> deleteFromListByIndexIfCan l i) l indicesForDeletion
            afterDeletionsInRAL = foldl (\t i -> delete t i ) ral indicesForDeletion

-- internal for test:
{-insertToListByIndex l i v
    | i>=0 && i<=length l = let (b,e) = splitAt i l in b++(v:e)
    | otherwise = l -} --to do: delete

insertToListByIndexIfCan l i _ | i<0 = l
insertToListByIndexIfCan l 0 v = v:l
insertToListByIndexIfCan [] _ _ = []
insertToListByIndexIfCan (h:l) i v = h:insertToListByIndexIfCan l (i-1) v

deleteFromListByIndexIfCan l i | i<0 = l
deleteFromListByIndexIfCan (h:l) 0 = l
deleteFromListByIndexIfCan [] _ = []
deleteFromListByIndexIfCan (h:l) i = h:deleteFromListByIndexIfCan l (i-1)

maxAmountOfDeleteOrInsertAtTest = 500

maxInsertionAmountWithStdgen maxAmount stdgen = randomR (0, maxAmount_) stdgen ::(Int, StdGen) where maxAmount_ = max (min maxAmount maxAmountOfDeleteOrInsertAtTest) 0
insertPositionsWithServiceData initialSourceLength rndPair  =
    iterate (genNextInGrowRange initialSourceLength) (((fst rndPair)::Int, snd rndPair), initialSourceLength) where
        genNextInGrowRange min_ ((nextValue, rnd), max_) = ((randomR (min_, max_) rnd), max_ + 1)
insertPositions_ initialSourceLength rndPair = map (fst . fst) $ insertPositionsWithServiceData initialSourceLength rndPair
insertPositions initialSourceLength stdgenForMove = (insertPositions_ initialSourceLength $
    (randomR (0, initialSourceLength) stdgenForMove))::[Int]

maxDeletionAmountWithStdgen maxAmount initialSourceLength stdgen | initialSourceLength > 0 = randomR (0, maxAmount_) stdgen ::(Int, StdGen) where maxAmount_ = min (max (min maxAmount maxAmountOfDeleteOrInsertAtTest) 0) (initialSourceLength-1)
deletePositionsWithServiceData initialSourceLength rndPair  = iterate (genNextInDecrescentRange 0) (((fst rndPair)::Int, snd rndPair), initialSourceLength-2) where
        genNextInDecrescentRange min_ ((nextValue, rnd), max_) = ((randomR (min_, max_) rnd), max_ - 1)
deletePositions_ initialSourceLength rndPair = map (fst . fst) $ deletePositionsWithServiceData initialSourceLength rndPair
deletePositions initialSourceLength stdgenForMove = (deletePositions_ initialSourceLength $
    (randomR (0, initialSourceLength-1) stdgenForMove))::[Int]

{-getl l i | i<0 = Nothing
        | i >= length l = Nothing
        | otherwise = Just $ head $ snd $ splitAt i l-} --to do: delete

getl (h:l) i    | i<0 = Nothing
                | i==0 = Just h
                | otherwise = getl l (i-1)
getl [] _ = Nothing

-- test for test internal:
should_insertPositions_in_range maxInsertionAmount initialSourceLength seed =
    (maxInsertionAmount>=0 && initialSourceLength>=0) ==>
    let (insertionAmount, stdgen) = maxInsertionAmountWithStdgen maxInsertionAmount (mkStdGen seed) in
        all (inRange (0, initialSourceLength+insertionAmount)) $ take insertionAmount $ insertPositions initialSourceLength stdgen

should_deletePositions_in_range maxDeletionAmount initialSourceLength seed =
    (maxDeletionAmount>=0 && initialSourceLength>0) ==>
    let (deletionAmount, stdgen) = maxDeletionAmountWithStdgen maxDeletionAmount initialSourceLength (mkStdGen seed) in
        all (inRange (0, initialSourceLength-1)) $ take deletionAmount $ deletePositions initialSourceLength stdgen

should_insert_on_index_at_list  :: [Int]->Int->Int->Bool
should_insert_on_index_at_list l i v =
    if i>=0 && i<=length l then length nl == (length l) + 1 && (nl!!i == v) else length nl == length l
    where nl = insertToListByIndexIfCan l i v

--test list:
main = do
    mapM_ quickCheck ([
        should_insertPositions_in_range
        ,should_deletePositions_in_range
        ]::[Int->Int->Int->Property])
    quickCheck should_insert_on_index_at_list
    mapM_ quickCheck ([
        should_be_equality_comparable
        ,should_be_ordering_comparable
        ]::[[Int]->[Int]->Bool])
    quickCheck should_construct_RAL_from_list
    mapM_ quickCheck ([
        should_get_item_by_index_when_exist
        , should_back_addable
        , should_front_addable
        ]::[[Int]->Int->Bool])
    mapM_ quickCheck ([
        should_back_popable
        ,should_front_popable
        ]::[[Int]->Property])
    quickCheck should_change_item_by_index
    mapM_ quickCheck ([
        should_allow_random_insertions
        ,should_allow_random_deletions
        ]::[[Int]->Int->Int->Property])
    quickCheck should_be_getable_after_random_insertions
    -- to do: Make tests to lrotate\rrotate\check_and_fix_balance
    -- and add tests to insert и delete and some others.
    print "Final"

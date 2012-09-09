module Forest () where

import Trees
import Random
import Array

-------------------------------------------------------------------------------
-- Impurity Scoring
-------------------------------------------------------------------------------
{-
 - Computes gini impurity score for Bool list.
 -}
gini_score :: [Bool] -> Float
gini_score list =
	1 - ((ts * ts + fs * fs)/(len*len))
	where
		len = (fromInteger . toInteger . length) list
		fs = len - ts
		ts = foldl (\y x->y+(if x then 1.0 else 0.0)) 0.0 list

-------------------------------------------------------------------------------
-- Stopping Tests
-------------------------------------------------------------------------------

{-
 - A stop test that does nothing.
 -}
no_stop_test :: (Int, Int, Int) -> Int -> Bool
no_stop_test stop_stats num_inputs = False

{-
 - Stop test that takes in max height, max nodes, max leaves, min inputs
 - and stops if those are reached.
 -}
hnli_stop_test :: (Int, Int, Int) -> Int -> (Int, Int, Int) -> Int -> Bool
hnli_stop_test (hs, ns, ls) is =
	\(ht, nt, lt) it -> ht >= hs || nt >= ns || lt >= ls || it <= is

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

{-
 - Returns random sequence of floats in the interval [0, 1]
 -}
rand_floats :: Int -> [Float]
rand_floats seed =
		randomRs (0.0, 1.0) g :: [Float]
	where
		g = mkStdGen seed

{-
 - Mergesorts a list, discarding duplicates.
 -}
mergesort_u :: Ord a => [a] -> [a]
mergesort_u [] = []
mergesort_u [x] = [x]
mergesort_u list =
	merge_u s1 s2
	where
		s1 = mergesort_u l1
		s2 = mergesort_u l2
		(l1, l2) = merge_split list

{-
 - Splts a list into two equal sublists.
 -}
merge_split :: [a] -> ([a], [a])
merge_split list =
	(front, back)
	where
		front = take half_length list
		back = drop half_length list
		half_length = (length list) `div` 2

{-
 - Performs a duplicate-discarding merge on two sorted lists.
 -}
merge_u :: Ord a => [a] -> [a] -> [a]
merge_u [] ys = ys
merge_u xs [] = xs
merge_u (x:xs) (y:ys)
	| x < y = x : (merge_u xs (y:ys))
	| otherwise = y : (merge_u (x:xs) ys)

{-
 - Inserts into a sorted list.
 -}
insertSorted :: Ord a => a -> [a] -> [a]
insertSorted x list =
	pre ++ (x:post)
	where
		pre = takeWhile (<x) list
		post = dropWhile (<x) list

{-
 - Inserts into a sorted list, maintaining uniqueness.
 -}
insertSorted_u :: Ord a => a -> [a] -> [a]
insertSorted_u x list =
	pre ++ (x:post)
	where
		pre = takeWhile (<x) list
		post = dropWhile (<=x) list

{-
 - Insertion sort.
 -}
insertionsort :: Ord a => [a] -> [a]
insertionsort = foldr insertSorted []

{-
 - Insertion sort, discarding duplicates.
 -}
insertionsort_u :: Ord a => [a] -> [a]
insertionsort_u = foldr insertSorted_u []

{-
 - Picks an element from the list given the float in [0.0, 1.0].
 - Slightly biased to the final element because it's not [0, 1).
 -}
rand_pick :: [a] -> Float -> a
rand_pick list float =
	list !! int
	where
		int = min (floor ((float *) $ fromInteger $ toInteger len))
			(len-1)
		len = length list

{-
 - Gives the fraction of the elements of the list that satisfy a predicate.
 -}
perc_fit :: Fractional b => (a->Bool) -> [a] -> b
perc_fit test list =
	lfit/lall
	where
		lfit = fromInt (length (filter test list))
		lall = fromInt (length list)

{-
 - The infamous fromInt function!!
 -}
fromInt :: (Integral a, Num b) => a -> b
fromInt = fromInteger . toInteger

-------------------------------------------------------------------------------
-- Arrays as inputs
-------------------------------------------------------------------------------

{-
 - Given the number of variables to pick, a list of random floats
 - in [0.0, 1.0], and the inputs, returns the new list of random floats
 - and the splitting functions.
 -}
rand_var_splits :: (Ix ix, Ord val) => Int -> [Float] ->
	[(Array ix val, Bool)] -> ([Float], [(Array ix val -> Bool)])
rand_var_splits m rand inputs =
	(newrand, splitfuncs)
	where
		inputvecs = fst $ unzip inputs
		(newrand, rindices) = (rand_ixs m rand inputvecs)
		splitfuncs = split_on rindices inputvecs

{-
 - Picks random indices, discarding duplicates.
 -}
rand_ixs :: Ix ix => Int -> [Float] -> [Array ix val] -> ([Float], [ix])
rand_ixs m rand inputs =
	(newrand, rindices)
	where
		rindices = foldr
			(\x y-> insertSorted_u (rand_pick inputrange x) y)
			[] mrand
		mrand = take m rand
		newrand = drop m rand
		inputrange = indices (head inputs)

{-
 - Takes a list of ords, and returns the splitting functions using (<)
 -}
all_splits :: Ord a => [a] -> [a->Bool]
all_splits vals =
	map (\x -> (\y -> y < x)) splitpoints
	where
		splitpoints = tail sorted_uniques
		sorted_uniques = mergesort_u vals

{-
 - Given a list of unique indices and a list of inputs, produces the splitting
 - functions on those indices in those inputs.
 -}
split_on :: (Ix ix, Ord val) => [ix] -> [Array ix val] ->
	[(Array ix val -> Bool)]
split_on indices inputs =
	foldr (\i acc -> (single_split_on inputs i) ++ acc) [] indices

{-
 - Creates splitting functions for arrays using a particular index
 -}
single_split_on :: (Ix ix, Ord val) => [Array ix val] -> ix ->
	[(Array ix val -> Bool)]
single_split_on inputs index =
	map (\f -> \arr -> f (arr ! index)) valsplits
	where
		valsplits = all_splits valsatindex
		valsatindex = map (! index) inputs

-------------------------------------------------------------------------------
-- Tree Growing Functions
-------------------------------------------------------------------------------

{-
 - Creates a random tree for a random forest.
 -}
rand_tree :: (Ix ix, Ord val) => [(Array ix val, Bool)] -> Int -> Int ->
	(Tree (Array ix val))
rand_tree inputs m seed
	| not $ samerange inputs = error "Inputs have different ranges!"
	| otherwise = the_tree
	where
		(the_tree, floats) = create_tree (
			stopping_test, gini_score, funcpoolgen, rand) inputs
		rand = rand_floats seed
		funcpoolgen = rand_var_splits m
		stopping_test = hnli_stop_test (5, 20, 20) 1

{-
 - Checks that arrays all have same range
 -}
samerange :: (Ix ix, Ord val) => [(Array ix val, Bool)] -> Bool
samerange (x:xs) = fst $ (foldl
	(\(bool, xrange) y -> ((bool && ((bounds $ fst y) == xrange)), xrange))
	(True, bounds $ fst x) xs)

replacement_sample :: [Float] -> [a] -> ([Float], [a])
replacement_sample rnd vals =
	(backrnd, foldr (\x y -> (rand_pick vals x):y) [] frontrnd)
	where
		n = length vals
		frontrnd = take n rnd
		backrnd = drop n rnd

-------------------------------------------------------------------------------
-- Forest Functions
-------------------------------------------------------------------------------

{-
 - Has the forest take a vote on an input.
 -}
forest_classify :: (Ix ix, Ord val) => [(Tree (Array ix val))] ->
	(Array ix val) -> Float
forest_classify forest input =
	true_vote
	where
		bools = map ((flip classify) input) forest
		true_vote = perc_fit id bools

rand_forest :: (Ix ix, Ord val) => [(Array ix val, Bool)] -> Int -> Int ->
	Int -> [(Tree (Array ix val))]
rand_forest inputs m seed numtrees =
	snd $
	foldr
		(\x (oldrand, oldforest) ->
		let
		(the_tree, newrand) = create_tree (stopping_test,
			gini_score, funcpoolgen, oldrand) inputs
		newforest = the_tree:oldforest
		in
		(newrand, newforest)
		)
		(rand, []) (replicate numtrees 0)
	where
		rand = rand_floats seed
		funcpoolgen = rand_var_splits m
		stopping_test = hnli_stop_test (5, 20, 20) 1

-------------------------------------------------------------------------------
-- In Progress
-------------------------------------------------------------------------------

run_forest :: (Ix ix, Ord val) => [(Array ix val, Bool)] -> Int -> Int ->
	Int -> (Float, [(Bool, Float)])
run_forest inputs m seed numtrees =
	(overall_acc, zip ys predicts)
	where
		(xs, ys) = unzip inputs
		the_forest = rand_forest inputs m seed numtrees
		predicts = map (forest_classify the_forest) xs
		abspredicts = map (>0.5) predicts
		corrects = map (uncurry (==)) (zip ys abspredicts)
		overall_acc = perc_fit id corrects

test_ins5 :: [Array Int Int]
test_ins5 =
	map (listArray (1, 5)) x5s
	where
		x5s =
		 [
			[0,0,0,0,0],
			[0,0,0,0,1],
			[0,0,1,0,1],
			[0,1,2,3,4],
			[1,1,1,1,1],
			[1,1,1,1,2],
			[1,1,2,1,2],
			[1,2,3,4,5],
			[2,2,2,2,2],
			[2,2,2,2,3],
			[2,2,3,2,3],
			[2,3,4,5,6],
			[3,3,3,3,3],
			[3,3,3,3,4],
			[3,3,4,3,4],
			[3,4,5,6,7],
			[4,4,4,4,4],
			[4,4,4,4,5],
			[4,4,5,4,5],
			[4,5,6,7,8],
			[5,5,5,5,5],
			[5,5,5,5,6],
			[5,5,6,5,6],
			[5,6,7,8,9],
			[6,6,6,6,6],
			[6,6,6,6,7],
			[6,6,7,6,7],
			[6,7,8,9,0],
			[7,7,7,7,7],
			[7,7,7,7,8],
			[7,7,8,7,8],
			[7,8,9,0,1],
			[8,8,8,8,8],
			[8,8,8,8,9],
			[8,8,9,8,9],
			[8,9,0,1,2],
			[9,0,1,2,3],
			[9,9,0,9,0],
			[9,9,9,9,0],
			[9,9,9,9,9]
		 ]
test_func1 :: Array Int Int -> Bool
test_func1 arr =
	a1 * a2 + a3 * a3 < a4 * a4 + a5 * a5
	where
		a1 = (arr ! 1)
		a2 = (arr ! 2)
		a3 = (arr ! 3)
		a4 = (arr ! 4)
		a5 = (arr ! 5)

test1 :: Int -> Int -> Int -> (Float, [(Bool, Float)])
test1 m seed numtrees =
	run_forest inputs m seed numtrees
	where
		inputs = zip xs ys
		xs = test_ins5
		ys = map test_func1 xs

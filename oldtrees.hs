module Trees (
	Tree(), isLeaf, getLeafVal,
	isNode, getNodeFunc, getNodeLeft, getNodeRight,
	getTreeHNL, getTreeHeight, getTreeNodes, getTreeLeaves,
	Stopping_Stats(),
	classify, validation, create_tree, test_ctree)
	where

{- To Do:
 -	Consider how to change so as to incorporate a function that produces
 	a function pool into the tree growth
 -	Extend to use arrays
	- Use different module, import this one
	- Write function that expects array inputs (of same range) and
	  produces a function pool for those array inputs
	- Start by writing a function that produces a function pool for
	  one variable (use <-, > is thus implied)
	- Continue by creating a function that produces a function pool for
	  two variables (A&&B and A&&notB, the other 2 quadrants are implied)
	- Continue by extending this to arbitrary Ord variables
 -	Change into random forests
	- Create a function that randomly selects m variables and splits
	  on them
	- Do out of bag stuff
 - 	Create diagnostics struct
 -}
{-
diagnose :: ((Float, Int), (Float, Int), (Int, Int, Int)) -> String
diagnose ((train_score, train_length), (test_score, test_length),
	(height, nodes, leaves))
	| leaves near nodes = "Maybe overfit, try reducing the f_pool"
	| leaves near train_length = ditto
	| test_score << train_score = ditto
	| test_score <0.5 = ditto
	| height too small = "Try increasing the f_pool"
	| train_score <0.7
	| nodes too few = "check that the inputs are varied enough"
-}


-------------------------------------------------------------------------------
-- Data Type Tree
-------------------------------------------------------------------------------
{-
 - Represents a tree of decision nodes (decision function, left side
 - the False branch and right side True branch) and leaves.
 -}
data Tree a =
	Node (a-> Bool) (Tree a) (Tree a) |
	Leaf Bool

isLeaf :: Tree a -> Bool
isLeaf (Leaf val) = True
isLeaf _ = False

isNode :: Tree a -> Bool
isNode (Node f l r) = True
isNode _ = False

getLeafVal :: Tree a -> Maybe Bool
getLeafVal tree =
	case tree of
		(Leaf val) -> Just val
		_ -> Nothing

getNodeFunc :: Tree a -> Maybe (a -> Bool)
getNodeFunc tree =
	case tree of
		(Node f l r) -> Just f
		_ -> Nothing

getNodeLeft :: Tree a -> Maybe (Tree a)
getNodeLeft tree =
	case tree of
		(Node f l r) -> Just l
		_ -> Nothing

getNodeRight :: Tree a -> Maybe (Tree a)
getNodeRight tree =
	case tree of
		(Node f l r) -> Just r
		_ -> Nothing

{-
 - Gives height, nodes, and leaves of a tree. These give some idea of the
 - complexity of the model.
 - The more leaves there are, the more regions in the input space have been
 - joined together by the tree. If there are too many, the model may be
 - overfitting.
 - If the tree is small but the accuracy is low, then the function pool may
 - have to be expanded, or the training set dependents should be checked to
 - see if they are sufficiently varied.
 -}
getTreeHNL :: Tree a -> (Int, Int, Int)
getTreeHNL (Leaf _) = (1, 0, 1)
getTreeHNL (Node _ l r) =
	(height, nodes, leaves)
	where
		(h_l, n_l, l_l) = getTreeHNL l
		(h_r, n_r, l_r) = getTreeHNL r
		height = 1 + max h_l h_r
		nodes = 1 + n_l + n_r
		leaves = l_l + l_r

getTreeHeight :: Tree a -> Int
getTreeHeight (Leaf _) = 1
getTreeHeight (Node _ l r) = 1 + max (getTreeHeight l) (getTreeHeight r)

getTreeNodes :: Tree a -> Int
getTreeNodes (Leaf _) = 0
getTreeNodes (Node _ l r) = 1 + (getTreeNodes l) + (getTreeNodes r)

getTreeLeaves :: Tree a -> Int
getTreeLeaves (Leaf _) = 1
getTreeLeaves (Node _ l r) = (getTreeLeaves l) + (getTreeLeaves r)

{-
 - Represents characteristics of the tree being built that can be queried by
 - custom stopping predicates.
 -}
data Stopping_Stats =
	Stopping_Stats {
		ss_Height_ :: Int,
		ss_LenInputs_ :: Int
	}
	deriving (Eq, Show, Read)

-------------------------------------------------------------------------------
-- Tree Creation and Use Functions
-------------------------------------------------------------------------------
{- Public
 - Slides an input down a tree and classifies it as either true or false.
 -}
classify :: Tree a -> a -> Bool
classify (Leaf ans) _ = ans
classify (Node f left right) input
	| goright = classify right input
	| otherwise = classify left input
	where
		goright = f input

{- Public
 - Classifies a validation set, returning the accuracy and the predictions
 -}
validation :: Tree a -> [(a, Bool)] -> (Float, [Bool])
validation tree validation_set =
	(accuracy, predictions)
	where
		(inputs, actuals) = unzip validation_set
		predictions = map (classify tree) inputs
		matches = map (uncurry (==)) (zip predictions actuals)
		accuracy = norm_score matches

{- Public
 - Takes a stopping test, a list of splitting functions, and a list of
 - training inputs, and produces a tree and an accuracy score for that
 - tree on the training inputs.
 -}
create_tree :: (Stopping_Stats->Bool)-> [(a -> Bool)] -> [(a, Bool)] ->
	(Tree a, Float)
create_tree stop_test funcs inputs =
	grow_tree stop_test init_stop_stats funcs inputs
	where
		init_stop_stats = Stopping_Stats 0 (length inputs)

{- Public
 - This tests growing a tree.
 - Takes a function pool, training set, and test set, grows a tree, and
 - reports training accuracy and size of training set, test accuracy and size
 - of test set, and height/nodes/leaves.
 -}
test_ctree :: (Stopping_Stats->Bool) -> [(a->Bool)] ->
	[(a, Bool)] -> [(a, Bool)] ->
	((Float, Int), (Float, Int), (Int, Int, Int))
test_ctree stop_test func_pool train_points test_points =
	((train_score, length train_points), (test_score, length test_points),
		hnl)
	where
		(tree, train_score) =
			create_tree stop_test func_pool train_points
		(test_score, predictions) = validation tree test_points
		hnl = getTreeHNL tree

-------------------------------------------------------------------------------
-- Private Functions
-------------------------------------------------------------------------------

{-
 - Takes a list of splitting functions and a list of training inputs and
 - produces a tree and an accuracy score for that tree on those inputs.
 - It operates recursively.
 -
 - Stops when there are no more useable functions (functions that split the
 - input - non-splitting functions are discarded), the stopping test is met,
 - or if all inputs are of one category.
 -}
grow_tree ::  (Stopping_Stats->Bool) -> Stopping_Stats ->
	[(a -> Bool)] -> [(a, Bool)] -> (Tree a, Float)
grow_tree stop_test stop_stats funcs inputs
	| stop_test stop_stats = use_leaf -- Stopping test is true
	| single_category inputs = use_leaf -- Input is of same category
	| length useable_funcs == 0 = use_leaf -- No functions split input
	| otherwise =
		(Node best_f left_tree right_tree, node_acc)
	where
		useable_funcs = filter (splits_inputs inputs) funcs
		use_leaf = best_leaf inputs
		(best_f, rest_fs) = find_best useable_funcs inputs
		(l_inputs, r_inputs) = filter_lr best_f inputs
		l_stop_stats = update_stop_stats stop_stats l_inputs
		r_stop_stats = update_stop_stats stop_stats r_inputs
		(left_tree, left_acc) =
			grow_tree stop_test l_stop_stats rest_fs l_inputs
		(right_tree, right_acc) =
			grow_tree stop_test r_stop_stats rest_fs r_inputs
		left_len = length l_inputs
		right_len = length r_inputs
		node_acc = node_accuracy left_acc right_acc left_len right_len

{-
 - Updates the stopping statistics by adding to height and recomputing inputs.
 -}
update_stop_stats :: Stopping_Stats -> [(a, Bool)] -> Stopping_Stats
update_stop_stats ss inputs
	= ss{ ss_Height_=newHeight, ss_LenInputs_=newLenInputs}
	where
		newHeight = 1 + ss_Height_ ss
		newLenInputs = length inputs

{-
 - Calculates the accuracy for a node, given the accuracies of its branches
 - and the number of inputs sent to each.
 -}
node_accuracy :: Float -> Float -> Int -> Int -> Float
node_accuracy l_acc r_acc l_len r_len =
	(l_acc * l_lenf + r_acc * r_lenf)/t_lenf
	where
		l_lenf = intToFloat l_len
		r_lenf = intToFloat r_len
		t_lenf = l_lenf + r_lenf
{-
 - Converts an int to a float.
 -}
intToFloat :: Int -> Float
intToFloat = fromInteger . toInteger

{-
 - Returns the function with the highest score purity score, and the other
 - functions. If there is a tie on the scores, it returns the later one.
 - It assumes that there is at least one function.
 -}
find_best :: [(a->Bool)] -> [(a, Bool)] -> ((a -> Bool), [(a -> Bool)])
find_best funcs inputs = find_best_acc_ [] (head fs) (tail fs)
	where
		scores = map (split_score parent_score inputs) funcs
		fs = zip funcs scores
		parent_score = (gini_score . snd . unzip) inputs

{- Accumulator for find_best, performing the search over the tuples -}
find_best_acc_ :: [(a -> Bool)] -> ((a -> Bool), Float) ->
	[((a -> Bool), Float)] -> ((a->Bool), [(a->Bool)])
find_best_acc_ rest (bestf, bests) [] = (bestf, rest)
find_best_acc_ rest (f1, s1) ((f2, s2):others)
	| s1 > s2 = find_best_acc_ (f2:rest) (f1, s1) others
	| otherwise = find_best_acc_ (f1:rest) (f2, s2) others

{-
 - Returns the most appropriate True/False leaf (tie is True) and the accuracy
 - of that leaf.
 -}
best_leaf :: [(a, Bool)] -> (Tree a, Float)
best_leaf inputs
	| nscore < 0.5 = (Leaf False, 1-nscore)
	| otherwise = (Leaf True, nscore)
	where
		(_, deps) = unzip inputs
		nscore = norm_score deps

{-
 - Returns whether these inputs are split by the function.
 -}
splits_inputs :: [(a, Bool)] -> (a->Bool) -> Bool
splits_inputs inputs func =
	not $ list_same result
	where
		result = map func indeps
		indeps = (fst . unzip) inputs

{-
 - Reports whether a training set is all of one category.
 - Assumes the list is of at least length 1.
 -}
single_category :: [(a, Bool)] -> Bool
single_category =
	list_same . snd . unzip

{-
 - Returns whether a list is composed of the same element.
 - Assumes the list is of at least length 1.
 -}
list_same :: Eq a => [a] -> Bool
list_same (x:xs) =
	fst $ foldl (\(acc, val) nval -> (acc&&val==nval,val)) (True, x) xs

{-
 - Takes a splitting function and splits the training set into left and
 - right. While this is a simple function, it is used in case the internal
 - structure of a tree changes, and is slightly more efficient.
 -}
filter_lr :: (a -> Bool) -> [(a, Bool)] -> ([(a, Bool)], [(a, Bool)])
filter_lr = filter_lr_acc_ [] []

{- Accumulator for filter_lr -}
filter_lr_acc_ :: [(a, Bool)] -> [(a, Bool)] -> (a -> Bool) -> [(a, Bool)] ->
	([(a, Bool)], [(a, Bool)])
filter_lr_acc_ left right func [] = (left, right)
filter_lr_acc_ left right func (x:xs)
	| func input = filter_lr_acc_ left (x:right) func xs
	| otherwise = filter_lr_acc_ (x:left) right func xs
	where
		(input, _) = x

{-
 - Computes gini impurity score for Bool list.
 -}
gini_score :: [Bool] -> Float
gini_score list =
	1 - ((ts * ts + fs * fs)/(len*len))
	where
		len = (intToFloat . length) list
		fs = len - ts
		ts = foldl (\y x->y+(if x then 1.0 else 0.0)) 0.0 list

{-
 - Gives a an improvement score (from Gini) for a function given inputs.
 - Note that it takes the parent score so as to be more efficient; however
 - this means that the calling function must know the purity scoring function.
 -}
split_score :: Float -> [(a, Bool)] -> (a->Bool) -> Float
split_score parent_score inputs f =
	parent_score - child_average_score
	where
		child_average_score = (r_score * r_len + l_score * l_len)/len
		r_score = gini_score r_class
		l_score = gini_score l_class
		len = (intToFloat . length) inputs
		r_len = (intToFloat . length) r_class
		l_len = (intToFloat . length) l_class
		r_class = snd $ unzip list_r
		l_class = snd $ unzip list_l
		(list_l, list_r) = filter_lr f inputs

{-
 - Counts the number of trues in the list.
 -}
abs_score :: [Bool] -> Float
abs_score = foldl (\x y -> x + if y==True then 1.0 else 0.0) 0.0

{-
 - Gives the proportion of the list that is true, range [0, 1]
 -}
norm_score :: [Bool] -> Float
norm_score match =
	hits / total
	where
		hits = abs_score match
		total = (fromInteger.toInteger) (length match)


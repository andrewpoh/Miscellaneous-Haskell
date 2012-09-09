-- From 7 August 2004

module Trees_Test (
	alltests,
	create_tree_test, leaf_test,
	ct_run)
	where

import qualified Trees

alltests :: Bool
alltests =
	create_tree_test &&
	leaf_test

epsilon :: Float
epsilon = 0.00001

close_enough :: Float -> Float -> Bool
close_enough x y =
	(x - y) < epsilon

create_tree_test :: Bool
create_tree_test = score `close_enough` exp
	where
		(tree, score) =
			Trees.create_tree (\x->False) function_pool inputs
		function_pool = [\x->x==0, \x->x<0]
		inputs = [(1, True), (2, True), (3, True), (4, False),
			(-1, False), (-2, False), (-3, False), (-4, False)]
		exp = 0.875

leaf_test :: Bool
leaf_test =
	Trees.getLeafVal tree == (Just False)
	where
		(tree, score) =
			Trees.create_tree (\x->False) function_pool inputs
		function_pool = []
		inputs = [(1, False), (2, False), (3, True), (4, False)]

ct_run :: ((Float, Int), (Float, Int), (Int, Int, Int))
ct_run = t_run true_func func_pool train_xs test_xs
	where
		true_func = true_function
		func_pool = function_pool
		train_xs = training_xs
		test_xs = testing_xs

-- Takes true function, function pool, training set, and test set
-- Produces ((training accuracy, training size), (test accuracy, test size),
-- (height, nodes, leaves))
t_run :: (a->Bool) -> [(a->Bool)] -> [a] -> [a] ->
	((Float, Int), (Float, Int), (Int, Int, Int))
t_run true_func func_pool train_xs test_xs = 
	Trees.test_ctree (\x->False) func_pool train_points test_points
	where
		train_points = zip train_xs train_ys
		train_ys = map true_func train_xs
		test_points = zip test_xs test_ys
		test_ys = map true_func test_xs

training_xs :: [(Float, Float)]
training_xs = [(0.0,0.0), (1.0,1.0), (-1.0,1.0), (-1.0,-1.0),
			(1.0,-1.0), (0.0,1.0), (0.0,-1.0), (1.0,0.0),
			(-1.0,0.0), (0.5, 0.5), (-0.5,-0.5), (-0.5,0.5),
			(0.5,-0.5), (0.7,-0.4), (0.4,0.7), (-0.3, 0.8),
			(0.4, 0.4)]

testing_xs :: [(Float, Float)]
testing_xs = [(0.4, 0.6), (1.1, -0.3), (0.5, -1.2), (0.4,0.2), (1.2, 0.9),
		(-0.8, -1.2), (-1.0,-0.2)]

function_pool :: [((Float,Float)-> Bool)]
function_pool = [\(x,y)->x<0.0, \(x,y)->y+x>1.6, \(x,y)->y<2.0,
		\(x,y)->abs x<0.5, \(x,y)->x>0.2]

true_function :: (Float,Float)-> Bool
true_function (x,y)
	| x < 0 && y + x > 1.6 && y < 2.0 = True
	| y < 0 && x < -0.5 = True
	| x > 0.2 && x < 0.5= True
	| x > 0.0 && y > x*x*x-0.3 = True
	| otherwise = False

-- From 17 March 2005

-- This causes ghc compiler to go into an infinite loop because
-- it infinitely inlines the function.
-- However it works in Hugs, and apparently there are no other
-- instances in which this causes an infinite loop.

newtype Wrap a = Wrap (Wrap a -> a)
self :: (Wrap a -> a) -> a
self x = x (Wrap x)

--y = \f -> (\g -> f (g g)) (\g -> f (g g))
-- Basically where there is a self-app, put the second one in Wrap
y =
	\f ->
	(\(Wrap g) -> f (g (Wrap g)))
	(Wrap (\(Wrap g) -> f (g (Wrap g))))

-- Alternatively
unwrap (Wrap a) = a

y2 =
	\f ->
	(\g -> f ((unwrap g) g))
	(Wrap (\g -> f ((unwrap g) g)))

facM :: (Integer -> Integer) -> Integer -> Integer
facM = \f -> \x -> if (x<1) then 1 else x * f (x-1)

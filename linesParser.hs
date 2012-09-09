-- Variation of "lines" that allows escaping via backslashes
-- 8-9 September 2012

import Data.List

-- Simple version that uses explicit recursion
parse :: String -> [String]
parse = parse_ False []

parse_ :: Bool -> String -> String -> [String]
parse_ _ rev [] = [reverse rev]
parse_ True rev (x:xs) = parse_ False (x:rev) xs
parse_ False rev (x:xs) =
	if x == '\\' then parse_ True rev xs
	else if x == '\n' then (reverse rev) : (parse_ False [] xs)
	else parse_ False (x:rev) xs

-- Foldl version that is tail strict
parseL :: String -> [String]
parseL text =
	let (_, finalWord, reversedWords) =
		foldl parseL_ (False, [], []) text in
	reverse ((reverse finalWord):reversedWords)

parseL_ :: (Bool, String, [String]) -> Char -> (Bool, String, [String])
parseL_ (True, rev, result) x = (False, x:rev, result)
parseL_ (False, rev, result) x =
	if x == '\\' then (True, rev, result)
	else if x == '\n' then (False, [], (reverse rev):result)
	else (False, x:rev, result)

-- Version that uses span and recursion
parseSpan :: String -> [String]
parseSpan text =
	let (l, r) = span (\c -> c /= '\\' && c /= '\n') text in
	if null r then [l]
	else
	let h = head r in
	let t = tail r in
	let ht = if null t then [] else [head t] in
	let tt = if null t then [] else tail t in
	if h == '\\' then appendToFirst (l++ht) (parseSpan tt)
	else l:(parseSpan t)

appendToFirst :: [a] -> [[a]] -> [[a]]
appendToFirst partial [] = [partial]
appendToFirst partial (x:xs) = (partial++x):xs

-- Version that uses unfold and span and also recursion
parseSpanLessRecursion :: String -> [String]
parseSpanLessRecursion = unfoldr spanner

spanner :: String -> Maybe (String, String)
spanner [] = Nothing
spanner text = 
	let (l, r) = span (\c -> c /= '\\' && c /= '\n') text in
	if null r then Just(l, [])
	else
	let h = head r in
	let t = tail r in
	let ht = if null t then [] else [head t] in
	let tt = if null t then [] else tail t in
	if h == '\\' then maybeAppend (l++ht) (spanner tt)
	else Just(l, t)

maybeAppend :: [a] -> Maybe ([a],[a]) -> Maybe ([a],[a])
maybeAppend partial Nothing = Just (partial, [])
maybeAppend partial (Just (pre,post)) = Just (partial++pre, post)

-- Version that uses span and no recursion
parseSpanNoRecursion :: String -> [String]
parseSpanNoRecursion = unfoldr spanMaxi

spanMaxi :: String -> Maybe (String, String)
spanMaxi [] = Nothing
spanMaxi text = Just $ snd $ (until fst spanMini (False, ([], text)))

spanMini :: (Bool, (String, String)) -> (Bool, (String, String))
spanMini (True, a) = (True, a)
spanMini (False, (previous, text)) =
	let (l, r) = span (\c -> c /= '\\' && c /= '\n') text in
	if null r then (True, (l, []))
	else
	let h = head r in
	let t = tail r in
	let ht = if null t then [] else [head t] in
	let tt = if null t then [] else tail t in
	if h == '\\' then (False, (previous++l++ht, tt))
	else (True, (previous++l, t))

-- Version that uses a custom list combinator
parseCombinator :: String -> [String]
parseCombinator = combinator nonEscapedLines
	(Just . reverse . snd) (False, [])

nonEscapedLines :: (Bool, [Char]) -> Char
	-> ((Bool, [Char]), Maybe [Char])
nonEscapedLines (True, rev) x = ((False, x:rev), Nothing)
nonEscapedLines (False, rev) '\n' = ((False, []), Just (reverse rev))
nonEscapedLines (False, rev) '\\' = ((True, rev), Nothing)
nonEscapedLines (False, rev) x = ((False, x:rev), Nothing)

combinator :: (state -> element -> (state, Maybe result))
	-> (state -> Maybe result)
	-> state -> [element] -> [result]
combinator _ final s [] =
	case final s of
		Nothing -> []
		Just r -> [r]
combinator transition final s0 (x:xs) =
	let (s1, m) = transition s0 x in
	case m of
		Nothing -> combinator transition final s1 xs
		Just r -> r : (combinator transition final s1 xs)

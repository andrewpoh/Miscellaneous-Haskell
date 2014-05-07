
-- Binomial coefficients
binomial :: Int -> Int -> Double
binomial x y =
	let term1 = toRational $ productBetween (x - y + 1) x in
	let term2 = toRational $ productBetween 2 y in
	fromRational $ (term1 / term2)

-- Arbitrary precision product
productBetween :: Int -> Int -> Integer
productBetween low high =
	let x = toInteger low in
	let y = toInteger high in
	product [x .. y]

-- Hypergeometric distribution probability mass function
hypergeo :: Int -> Int -> Int -> Int -> Double
hypergeo matches total draws accepts =
	let term1 = binomial matches accepts in
	let term2 = binomial total draws in
	let term3 = binomial (total - matches) (draws - accepts) in
	term1 / term2 * term3

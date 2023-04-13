import Prelude


{------------- COMMON -------------}
fib :: Integer -> Integer
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib (n-1) + fib (n-2)

isPrime :: Integer -> Bool
isPrime n | n == 1 = False
          | n == 2 = True
          | otherwise = null [x | x <- [2..floor (sqrt (fromIntegral n))], n `mod` x == 0]

decomposeNumHelper :: Integer -> [Integer]
decomposeNumHelper n | n < 10 = [n]
                     | otherwise = (n `mod` 10) : decomposeNumHelper (n `div` 10)

decomposeNum :: Integer -> [Integer]
decomposeNum n = reverse (decomposeNumHelper n)

isPalindrome :: Integer -> Bool
isPalindrome i = decomposeNum i == reverse (decomposeNum i)

divisorsOf :: Integer -> [Integer]
divisorsOf n = [x | x <- [0..n], n `mod` x == 0]

areCoprimes :: Integer -> Integer -> Bool
areCoprimes n m = gcd n m == 1

integerToDouble :: Integer -> Double
integerToDouble n = fromIntegral n :: Double

{------------- PROBLEM 1 -------------}
problem_1 :: Integer
problem_1 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]


{------------- PROBLEM 2 -------------}
problem_2_even_set :: [Integer] -> [Integer]
problem_2_even_set xs = [i | i <- xs, even (fib i)]

problem_2_fib_set :: [Integer] -> [Integer]
problem_2_fib_set xs | null xs = problem_2_fib_set [0]
                     | fib (last xs) >= 4000000 = xs
                     | otherwise = problem_2_fib_set (xs ++ [last xs + 1])

problem_2 :: Integer
problem_2 = sum (map fib (problem_2_even_set (problem_2_fib_set [])))


{------------- PROBLEM 2 (alternative (I didn't know the existence of takeWhile)) -------------}
problem_2_alt :: Integer
problem_2_alt = sum [x | x <- takeWhile (<= 4000000) (map fib [0..]), even x]


{------------- PROBLEM 3 -------------}
problem_3 :: Integer
problem_3 = maximum [x | x <- [1..floor (sqrt 600851475143)], 600851475143 `mod` x == 0, isPrime x]

{------------- PROBLEM 4 -------------}
researchedPalindromes :: [(Integer, Integer, Integer)]
researchedPalindromes = [(x, y, x*y) | x <- [100..999], y <- [100..999], isPalindrome (x*y)]

maxReseachedPalindrome :: [(Integer, Integer, Integer)] -> Integer
maxReseachedPalindrome xs = maximum [z | (x, y, z) <- xs]

problem_4 :: Integer
problem_4 = maxReseachedPalindrome researchedPalindromes


{------------- PROBLEM 5 -------------}
satisfies_problem_5 :: Integer -> Integer
satisfies_problem_5 n | null [x | x <- [1..20], n `mod` x /= 0] = n
                      | otherwise = satisfies_problem_5 (n + 1)

problem_5 :: Integer
problem_5 = satisfies_problem_5 1


{------------- PROBLEM 6 -------------}
sumSquares :: Integer -> Integer
sumSquares n = sum [x | y <- [0..n], x <- [y*y]]

squareSums :: Integer -> Integer
squareSums n = sum [0..n] * sum [0..n]

problem_6 :: Integer
problem_6 = squareSums 100 - sumSquares 100



{------------- PROBLEM 570 -------------}
{- findThickness :: Integer -> Integer -> Integer
findThickness order thickness =  -}


{------------- PROBLEM 838 -------------}
problem_838_integersToTest :: Integer -> [Integer]
problem_838_integersToTest n = [m | m <- [0..n], last (decomposeNum m) == 3]

problem_838_testNotCoprime :: Integer -> Integer -> Int -> Bool
problem_838_testNotCoprime m n i | i + 1 >= length xs = True
                                 | not (areCoprimes m (xs !! i)) = problem_838_testNotCoprime m n (i + 1)
                                 | otherwise = False
                                 where xs = problem_838_integersToTest n

problem_838_findNotCoprime :: Integer -> Integer -> Integer
problem_838_findNotCoprime m n | problem_838_testNotCoprime m n 0 = m
                               | otherwise = problem_838_findNotCoprime (m + 1) n

problem_838_func_f :: Integer -> Integer
problem_838_func_f = problem_838_findNotCoprime 1

problem_838 :: Double
problem_838 = log (integerToDouble (problem_838_func_f 1000000))

main :: IO ()
main = print problem_838
limIters :: Integer
limIters = 2000000

f:: Integer -> Integer -> Integer
f x n = (x * x + 1) `mod` n

primeCheck :: Integer -> Integer -> Integer -> Bool
primeCheck a n pow2
    | a == 1                             = True 
    | pow2 == 0                          = a == 1
    | a /= n - 1 && (a * a) `mod` n == 1 = False
    | otherwise                          = primeCheck ((a * a) `mod` n) n (pow2 - 1)

getMaxDividingPow2 :: Integer -> Integer
getMaxDividingPow2 n
    | n `mod` 2 == 1 = 0
    | otherwise      = getMaxDividingPow2 (n `div` 2) + 1

powerModulo :: Integer -> Integer -> Integer -> Integer
powerModulo a b n
    | b == 0         = 1
    | b `mod` 2 == 0 = (pw * pw) `mod` n 
    | otherwise      = (a * powerModulo a (b - 1) n) `mod` n
        where 
            pw = powerModulo a (b `div` 2) n

isPrime :: Integer -> Integer -> Bool 
isPrime a n = primeCheck (powerModulo a ((n - 1) `div` (2 ^ pm)) n) n pm
                where 
                    pm = getMaxDividingPow2 (n - 1)

getNextRnd :: Integer -> Integer
getNextRnd n = (n * 307139 + 17) `mod` 891771648051262478234242325342735462117

prime :: Integer -> Integer -> Integer -> Bool 
prime rnd n steps
    | steps == 0                    = True
    | not (isPrime (rnd `mod` n) n) =  False
    | otherwise                     = prime (getNextRnd rnd `mod` (n - 1) + 1) n (steps - 1)

getDiv :: Integer -> Integer -> Integer -> Integer -> Integer
getDiv x y n k 
    | k == 0    = 1
    | g /= 1    = g 
    | otherwise = getDiv x (f y n) n (k - 1) 
        where 
            g = gcd (abs (x - y)) n
    

getRho :: Integer -> Integer -> Integer -> Integer 
getRho x k n
    | k == 0    = x 
    | otherwise = f (getRho x (k - 1) n) n

getDivisor :: Integer -> Integer -> Integer -> Integer
getDivisor rnd n k
    | k == 0        = 1
    | divisor /= 1  = divisor
    | otherwise     = getDivisor (getNextRnd rnd `mod` (n - 3)) n (k - 1)
        where 
            iters   = min limIters $ (floor.sqrt.sqrt) (fromInteger n :: Double)
            divisor = getDiv (getRho (rnd `mod` (n - 3) + 2) iters n) (rnd `mod` (n - 3) + 2) n iters

factor :: Integer -> [Integer]
factor n 
 | n == 1 = []
 | n `mod` 2  == 0 =  2 : factor (n `div`  2)
 | n `mod` 3  == 0 =  3 : factor (n `div`  3)
 | n `mod` 5  == 0 =  5 : factor (n `div`  5)
 | n `mod` 7  == 0 =  7 : factor (n `div`  7)
 | n `mod` 11 == 0 = 11 : factor (n `div` 11)
 | n `mod` 13 == 0 = 13 : factor (n `div` 13)
 | n `mod` 17 == 0 = 17 : factor (n `div` 17)
 | prime 2 n 30    = [n]
 | otherwise       = factor divisor ++ factor (n `div` divisor)
                       where
                           divisor = getDivisor 3 n 10


main :: IO()
main = print $ factor (2^100 + 1)

--3784523845862452382375472384768235462389568245672389468735476923856235
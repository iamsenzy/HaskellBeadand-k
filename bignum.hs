type Base   = Int
type BigNum = [Int]
type Granularity = Int


toBigNum :: Base -> Integer -> BigNum
toBigNum i 0 = [0]
toBigNum i a = toBigNum' (fromIntegral(i)) (fromIntegral(a))
toBigNum' i a
 | i<2 || a<0 = error ("toBigNum: improper arguments: " ++ show a ++ " " ++ show i)
 | (a>0) = (fromIntegral (a `mod` i)) : toBigNum' i  (a `div` i)
 | otherwise = []


fromBigNum :: Base -> BigNum -> Integer
fromBigNum b l = sum [(fromIntegral n) * (fromIntegral b)^k
                     | (n,k) <- zip l [0..] ]


pad :: ([a] -> [a] -> [a]) -> a -> Int -> [a] -> [a]
pad f e n l =f (pad' e (n-length l) []) l
pad' e n x
 |n>0 = pad' e (n-1) (e:x)
 |otherwise = x
 

padLeft :: a -> Int -> [a] -> [a]
padLeft e n l = pad (\p -> (p ++)) e n l


padRight :: a -> Int -> [a] -> [a]
padRight e n l = pad (\p -> (++ p)) e n l


(<=>) :: BigNum -> BigNum -> (BigNum, BigNum)
(<=>) x y
 |length x > length y = (x , padRight 0 (length x) y )
 |otherwise = (padRight 0 (length y) x , y )



addBigNums :: Base -> BigNum -> BigNum -> BigNum
addBigNums _ [] [] = []
addBigNums i [a] [b]
	| (a + b) >= c = ((a + b) `mod` c) : [ (a + b) `div` c ]
	| otherwise = [ a + b ]
	where
		c = fromIntegral i
addBigNums i x y
	| (a + b) >= c = ((a + b) `mod` c) : ( addBigNums i ((((fromIntegral . head) vs) + ((a + b) `div` c)) : (tail vs)) ws )
	| otherwise = (a + b) : ( addBigNums i vs ws )
	where
		pair = x <=> y
		(v:vs) = fst pair
		(w:ws) = snd pair
		a = fromIntegral v
		b = fromIntegral w
		c = fromIntegral i


sumBigNums :: Base -> [BigNum] -> BigNum
sumBigNums i [] = []
sumBigNums i [a]      = addBigNums i a [0]
sumBigNums i [a,b]    = addBigNums i a b
sumBigNums i (a:b:xs) =addBigNums i (addBigNums i a b) (sumBigNums i xs)


subBigNums :: Base -> BigNum -> BigNum -> BigNum
subBigNums _ l [] = l
subBigNums i l k
 |length a == 1 && y>x = [(i-(y-x)), -1]
 |x>=y = (x-y) : subBigNums i xs ys
 |y>x = (i-(y-x)) : subBigNums i ((z-1):zs) ys
 where
 (a@(x:xs),(y:ys)) = l<=>k
 (z:zs) = xs

diffBigNums :: Base -> [BigNum] -> BigNum
diffBigNums _ [] = []
diffBigNums i (x:xs) = foldl (subBigNums i) x xs


logPowerBase :: Base -> Int -> (Int, Int)
logPowerBase 0 _ = error "logPowerBase: improper arguments"
logPowerBase a b 
  |b>=a = (1+first, a*last)
  |otherwise = (0,1)
 where
  first = fst(logPowerBase a (b `div` a))
  last = snd(logPowerBase a (b `div` a))

powersOf :: Base -> Int -> [Int]
powersOf a 0 = []
powersOf a b = [fst(logPowerBase a b)]++powersOf a (b- (snd(logPowerBase a b)))



multBigNum :: Base -> Int -> BigNum -> BigNum
multBigNum _ _ [] = []
multBigNum _ 0 _ = [0]
multBigNum i a b = sumBigNums i [ padLeft 0 ((length b) + n ) b |  n <- (powersOf i a) ]

multBigNums :: Base -> BigNum -> BigNum -> BigNum
s _ [] _      = []
s i a []      = s i a [0]
s i (a:as) b = multBigNum i a b : s i as (0:b)
multBigNums i x y = sumBigNums i ( s i x y )



karatsuba :: Base -> Granularity -> BigNum -> BigNum -> BigNum
karatsuba b m x y
 | b < 1 ||m < 0 = error "karatsuba: Improper arguments"
 | (length x <= m1) || (length y <= m1) = multBigNums b x y
 | otherwise = sumBigNums b [c, d, z0]
  where
   m1 = fromIntegral m
   b1 = fromIntegral b
   (v , w) = x <=> y
   l = ceiling (fromIntegral(length v) / 2)
   (low1, high1) = splitAt l v
   (low2, high2) = splitAt l w
   z0 = karatsuba b m low1 low2
   z1 = karatsuba b m (addBigNums b low1 high1) (addBigNums b low2 high2)
   z2 = karatsuba b m high1 high2
   difference = diffBigNums b [z1,z2,z0]
   c = padLeft 0 ((length z2) + (2 * l)) z2
   d = padLeft 0 (length(difference) + (l)) (difference)


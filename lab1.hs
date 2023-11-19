module Lab where
import Data.Text.Internal.Builder.Int.Digits (digits)

-- Задание 1. Наибольший общий делитель
myGCD :: Integer -> Integer -> Integer
myGCD 0 b = b
myGCD a b = myGCD (x - y) y 
        where                                   
                x = max a b
                y = min a b     



-- Задание 2. Бинарное возведение в степень
binpow :: Integer -> Integer -> Integer
binpow a n = bpHelper a n 1

bpHelper :: Integer -> Integer -> Integer -> Integer
bpHelper _ 0 res = res
bpHelper a n res = bpHelper (a * a) (div n 2) z 
        where 
                z = if odd n then res * a else res



-- Задание 3. Вычисление чисел Фибоначчи за O(log n)
fib :: Integer -> Integer
fib n  = fibHelper [0, 1, 1, 1] n [1, 0, 0, 1]

fibHelper :: [Integer] -> Integer -> [Integer] -> Integer
fibHelper _ 0 res = res!!1 
fibHelper a n res = fibHelper (mult a a) (div n 2) z 
        where 
                z = if odd n then mult res a else res

mult :: [Integer] -> [Integer] -> [Integer]
mult a b = [x, y, z, w] 
        where
                x = a!!0 * b!!0 + a!!1 * b!!2
                y = a!!0 * b!!1 + a!!1 * b!!3
                z = a!!2 * b!!0 + a!!3 * b!!2
                w = a!!2 * b!!1 + a!!3 * b!!3



-- Задание 4. Совершенные числа
perfect :: Integer -> Bool
perfect n = sum x == n  
        where  
                x = [d | d <- [1 .. n `div` 2], mod n d == 0]



-- Задание 5. Совершенные числа
collatz :: Integer -> Integer
collatz n = cHelper n 0

cHelper :: Integer -> Integer -> Integer
cHelper 1 len = len + 1
cHelper n len = cHelper x (len + 1)
        where
                x = if even n then div n 2 else 3 * n + 1



-- Задание 6. Числа Деланнуа
delannoy :: Integer -> Integer -> Integer
delannoy 0 n = 1
delannoy m 0 = 1
delannoy m n = delannoy (m - 1) n + delannoy m (n - 1) + delannoy (m - 1) (n - 1)



-- Задание 7. Вычисление многочлена
evalPolynomial :: [Integer] -> Integer -> Integer
evalPolynomial coeff x =
  sum $ zipWith (\c power -> c * x ^ power) coeff [length coeff - 1, length coeff - 2 .. 0]



-- Задание 8. Клонирование элементов списка
clone :: Int -> [a] -> [a]
clone n a = concatMap (replicate n) a



-- Задание 9. Сшивание списков бинарной операцией
xZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
xZipWith _ [] _ = []
xZipWith _ _ [] = []
xZipWith f (x:xs) (y:ys) = f x y : xZipWith f xs ys



-- Задание 10. Список чисел Фибоначчи
fibList :: [Integer]
fibList = 0 : 1 : zipWith (+) fibList (tail fibList)

nFib :: Int -> [Integer]
nFib n = take n fibList 

genFib :: [Integer] -> [Integer]
genFib initial = map head $ iterate (\current -> tail current ++ [sum current]) initial



-- Задание 11. Системы счисления
fromDigits :: Int-> [Int] -> Int
fromDigits n digits = foldl (\acc digit -> acc * n + digit) 0 digits


toDigits :: Int -> Int -> [Int]
toDigits n x
  | x < n  = [x]
  | otherwise = toDigits n (div x n) ++ [mod x n]


addDigitwise :: Int -> [Int] -> [Int] -> [Int]
addDigitwise n digits1 digits2 = toDigits n (fromDigits n digits1 + fromDigits n digits2)



-- Задание 12. Перечесление путей в решетке
delannoyPaths :: Int -> Int -> [[Int]]
delannoyPaths a b = generatePaths 0 0
  where
    generatePaths x y
      | x == a && y == b = [[]]
      | otherwise = concat [if x < a then map (0 :) $ generatePaths (x + 1) y else [],
                            if x < a && y < b then map (1 :) $ generatePaths (x + 1) (y + 1) else [],
                            if y < b then map (2 :) $ generatePaths x (y + 1) else []]


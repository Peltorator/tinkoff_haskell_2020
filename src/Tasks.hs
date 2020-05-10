module Tasks where

import Prelude hiding (head, tail, take, drop, filter, foldl, foldr, concat, (++), zip, map)

-- В этом задании запрещается использовать какие-либо функции из стандартной библиотеки,
-- кроме конструкторов списков и операторов сравнения из `Ord`.
-- Также запрещается использовать list comprehension.
-- Однако разрешается использовать дополнительные функции, реализованные самостоятельно.

-- 1. head' возвращает первый элемент непустого списка
head' :: [a] -> a
head' (x:_) = x

-- 2. tail' возвращает список без первого элемента, для пустого - пустой
tail' :: [a] -> [a]
tail' []     = []
tail' (_:xs) = xs

-- 3. take' возвращает первые n >= 0 элементов исходного списка; если n больше длины списка, то все элементы
take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x:(take' (n - 1) xs)

-- 4. drop' возвращает список без первых n >= 0 элементов; если n больше длины списка, то пустой список.
drop' :: Int -> [a] -> [a]
drop' 0 a      = a
drop' _ []     = []
drop' n (x:xs) = drop' (n - 1) xs

-- 5. filter' возвращает список из элементов, для которых f возвращает True
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []                 = []
filter' f (x:xs) | f x       = x:(filter' f xs)
                 | otherwise = filter' f xs

-- 6. zip' принимает два списка [a1, a2, ...] и [b1, b2, ...] и возвращает список [(a1, b1), (a2, b2), ...].
-- Размер итогого списка равен размеру меньшего из входных списков.
zip' :: [a] -> [b] -> [(a, b)]
zip' xs []         = []
zip' [] ys         = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

-- 7. map' принимает на вход функцию и список и применяет функцию ко всем элементам списка
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x:map' f xs

-- 8. foldr' последовательно применяет функцию f с конца
-- foldr' (+) 0 [1, 2, 3] == (1 + (2 + (3 + 0)))
-- foldr' (*) 4 [] == 4
foldr' :: (b -> a -> a) -> a -> [b] -> a
foldr' _ a []     = a
foldr' f a (x:xs) = x `f` (foldr' f a xs)

-- 9. foldl' последовательно применяет функцию f с начала
-- foldl' (+) 0 [1, 2, 3] == (((0 + 1) + 2) + 3)
-- foldl' (*) 4 [] == 4
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ a []     = a
foldl' f a (x:xs) = foldl' f (a `f` x) xs

-- 10. concat' принимает на вход два списка и возвращает их конкатенацию
-- concat' [1,2] [3] == [1,2,3]
concat' :: [a] -> [a] -> [a]
concat' []     ys = ys
concat' (x:xs) ys = x:concat' xs ys

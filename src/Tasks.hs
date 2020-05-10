module Tasks where

import Prelude hiding (head, tail, take, drop, filter, foldl, foldr, concat, (++), zip, map)

-- В этом задании запрещается использовать какие-либо функции из стандартной библиотеки,
-- кроме конструкторов списков и операторов сравнения из `Ord`.
-- Также запрещается использовать list comprehension.
-- Однако разрешается использовать дополнительные функции, реализованные самостоятельно.

-- 1. head' возвращает первый элемент непустого списка
head' :: [a] -> a
head' (a:as) = a

-- 2. tail' возвращает список без первого элемента, для пустого - пустой
tail' :: [a] -> [a]
tail' []     = []
tail' (a:as) = as

-- 3. take' возвращает первые n >= 0 элементов исходного списка; если n больше длины списка, то все элементы
take' :: Int -> [a] -> [a]
take' 0 as     = []
take' n []     = []
take' n (a:as) = a : (take' (n - 1) (as))

-- 4. drop' возвращает список без первых n >= 0 элементов; если n больше длины списка, то пустой список.
drop' :: Int -> [a] -> [a]
drop' 0 as     = as
drop' n []     = []
drop' n (a:as) = drop' (n - 1) as

-- 5. filter' возвращает список из элементов, для которых f возвращает True
--check' True a = [a]
--check' False a = []
filter'' f False (a:as) = filter' f as
filter'' f True  (a:as) = a : filter' f as

filter' :: (a -> Bool) -> [a] -> [a]
filter' f []     = []
filter' f (a:as) = filter'' f (f a) (a:as) --(check' (f a) (a)) filter' (f) (as)

-- 6. zip' принимает два списка [a1, a2, ...] и [b1, b2, ...] и возвращает список [(a1, b1), (a2, b2), ...].
-- Размер итогого списка равен размеру меньшего из входных списков.
zip' :: [a] -> [b] -> [(a, b)]
zip' []     []     = []
zip' []     as     = []
zip' as     []     = []
zip' (a:as) (b:bs) = (a, b) : zip' as bs

-- 7. map' принимает на вход функцию и список и применяет функцию ко всем элементам списка
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (a:as) = (f a) : map' f as

-- 8. foldr' последовательно применяет функцию f с конца
-- foldr' (+) 0 [1, 2, 3] == (1 + (2 + (3 + 0)))
-- foldr' (*) 4 [] == 4
foldr' :: (b -> a -> a) -> a -> [b] -> a
foldr' f a []     = a
foldr' f a (b:bs) = f b (foldr' f a bs)

-- 9. foldl' последовательно применяет функцию f с начала
-- foldl' (+) 0 [1, 2, 3] == (((0 + 1) + 2) + 3)
-- foldl' (*) 4 [] == 4
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f a []     = a
foldl' f a (b:bs) = foldl' f (f a b) bs 


-- 10. concat' принимает на вход два списка и возвращает их конкатенацию
-- concat' [1,2] [3] == [1,2,3]
concat' :: [a] -> [a] -> [a]
concat' []     as = as
concat' (a:as) bs = a : concat' as bs

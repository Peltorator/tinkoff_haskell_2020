module Tasks3 where

-- Здесь нельзя использовать deriving кроме Show.



-- 17. Тренировочная задача на знакомств с пользовательскими типами. Дан тип данных Person:
data Person = Person { firstName :: String, lastName :: String, age :: Int }
    deriving (Show, Eq)
-- Функция abbrFirstName сокращает имя мя до первой буквы с точкой,
-- то есть если имя было "John", то после применения этой функции, оно превратится в "J.".
-- Однако если имя было короче двух символов, то оно не меняется.
-- P.S. Если что, строка -- список символов.
abbrFirstName :: Person -> Person
abbrFirstName = undefined


-- Определим наше дерево, которое мы далее будем использовать:

data Tree a = Nil | Node (Tree a) a (Tree a) 
    deriving Show

-- 18. Функция treeSum вычисляет сумму элементов дерва.
treeSum :: Tree Integer -> Integer
treeSum = undefined


-- 19. Функция treeHeight вычисляет максимальную высоту дерева.
treeHeight :: Tree a -> Int
treeHeight = undefined


-- 20. Сделайте Tree представителем класса типов Eq.
instance Eq a => Eq (Tree a) where
    (==) = undefined

-- Для реализации свертки двоичных деревьев нужно выбрать алгоритм обхода узлов дерева.
-- Сделайте дерево представителем класса типов Foldable несколькими способами.
-- Так как нельзя одно и то же дерево сделать Foldable несколькими способами,
-- мы заведем ему псевдонимы:
newtype Preorder a   = PreO   (Tree a) deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a) deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)
-- В данном контексте можно считать, что newtype -- это то же самое, что data,
-- Но конструктор данных только один. Это обертка над деревом.
--
-- Теперь сделайте 4 представителя класса типов Foldable:
-- Tree в порядке левое поддерево - вершина - правое поддерево;
-- Preorder в порядке вершина - левое поддерево - правое поддерево;
-- Postorder в порядке левое поддерево - правое поддерево - вершина;
-- Levelorder в порядке bfs (по уровням, на одном уровне -- слева направо).
--
-- 21.
instance Foldable Tree where
    foldr = undefined

-- 22.
instance Foldable Preorder where
    foldr = undefined

-- 23.
instance Foldable Postorder where
    foldr = undefined

-- 24.
instance Foldable Levelorder where
    foldr = undefined

-- 25. treeSum' вычисляет сумму элементов дерева. Примените foldr.
treeSum' :: Tree Integer -> Integer
treeSum' = undefined



-- Определим наш список, который мы далее будем использовать:
data MyList a = Empty | Cons a (MyList a)
    deriving Show

-- 26. Сделайте MyList представителем класса типов Eq.
instance Eq a => Eq (MyList a) where
    (==) = undefined

-- 27. Сделайте MyList представителем класса типов Ord. Достаточно реализовать оператор (<=).
instance Ord a => Ord (MyList a) where
    (<=) = undefined

-- 28. Сделайте MyList представителем класса типов Foldable.
instance Foldable MyList where
    foldr = undefined

-- 29. Сделайте MyList представителем класса типов Functor.
instance Functor MyList where
    fmap = undefined

-- 30. sum2D вычисляет сумму элементов двумерного списка.
-- Используйте реализованные выше instance'ы, чтобы сделать все в бесточечном стиле.
sum2D :: Num a => MyList (MyList a) -> a
sum2D = undefined

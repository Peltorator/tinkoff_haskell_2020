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
abbrFirstName p = Person (let name = firstName p in
                              if length name > 1
                              then head name : "."
                              else name)
                         (lastName p) 
                         (age p)


-- Определим наше дерево, которое мы далее будем использовать:

data Tree a = Nil | Node (Tree a) a (Tree a) 
    deriving Show

-- 18. Функция treeSum вычисляет сумму элементов дерва.
treeSum :: Tree Integer -> Integer
treeSum Nil                   = 0
treeSum (Node left val right) = treeSum left + val + treeSum right


-- 19. Функция treeHeight вычисляет максимальную высоту дерева.
treeHeight :: Tree a -> Int
treeHeight Nil                 = 0
treeHeight (Node left _ right) = max (treeHeight left) (treeHeight right) + 1


-- 20. Сделайте Tree представителем класса типов Eq.
instance Eq a => Eq (Tree a) where
    (==) Nil Nil                                           = True
    (==) Nil _                                             = False
    (==) _ Nil                                             = False
    (==) (Node left1 val1 right1) (Node left2 val2 right2) = (val1 == val2) && (left1 == left2) && (right1 == right2)

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
    foldr f init Nil                   = init
    foldr f init (Node left val right) = foldr f (f val (foldr f init right)) left

-- 22.
instance Foldable Preorder where
    foldr f init (PreO Nil)                   = init
    foldr f init (PreO (Node left val right)) = f val (foldr f (foldr f init (PreO right)) (PreO left))

-- 23.
instance Foldable Postorder where
    foldr f init (PostO Nil)                   = init
    foldr f init (PostO (Node left val right)) = foldr f (foldr f (f val init) (PostO right)) (PostO left)

-- 24.

instance Foldable Levelorder where
    foldr f init (LevelO t) = foldr f init (concat (levels t))
        where levels Nil                   = [[]]
              levels (Node left val right) = [val] : take (max (treeHeight left) (treeHeight right)) (zipWith (++) (levels left ++ repeat []) (levels right ++ repeat []))

-- 25. treeSum' вычисляет сумму элементов дерева. Примените foldr.
treeSum' :: Tree Integer -> Integer
treeSum' = foldr (+) 0



-- Определим наш список, который мы далее будем использовать:
data MyList a = Empty | Cons a (MyList a)
    deriving Show

-- 25. Сделайте MyList представителем класса типов Eq.
instance Eq a => Eq (MyList a) where
    (==) Empty Empty               = True
    (==) Empty _                   = False
    (==) _ Empty                   = False
    (==) (Cons a1 b1) (Cons a2 b2) = (a1 == a2) && (b1 == b2)

-- 26. Сделайте MyList представителем класса типов Ord. Достаточно реализовать оператор (<=).
instance Ord a => Ord (MyList a) where
    (<=) Empty Empty  = True
    (<=) Empty _      = True
    (<=) _ Empty      = False
    (<=) (Cons a1 b1) (Cons a2 b2)
          | a1 > a2   = False
          | a1 < a2   = True
          | otherwise = b1 <= b2

-- 27. Сделайте MyList представителем класса типов Foldable.
instance Foldable MyList where
    foldr f init Empty      = init
    foldr f init (Cons a b) = f a (foldr f init b)

-- 28. Сделайте MyList представителем класса типов Functor.
instance Functor MyList where
    fmap f Empty      = Empty
    fmap f (Cons a b) = Cons (f a) (fmap f b)

-- 29. sum2D вычисляет сумму элементов двумерного списка.
-- Используйте реализованные выше instance'ы, чтобы сделать все в бесточечном стиле.

sum2D :: Num a => MyList (MyList a) -> a
sum2D = sum . fmap sum

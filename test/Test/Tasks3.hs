module Test.Tasks3 where

import Tasks3 (Person (..), Tree (..), Preorder (..), Postorder (..), Levelorder (..), MyList (..), abbrFirstName, treeSum, treeHeight, treeSum', sum2D)

import Test.Tasty.HUnit (Assertion, (@?=))

unit_abbrFirstName :: Assertion
unit_abbrFirstName = do
    abbrFirstName (Person "Serj" "Obrit" 16) @?= (Person "S." "Obrit" 16)
    abbrFirstName (Person "S" "Obrit" 16)    @?= (Person "S" "Obrit" 16)
    abbrFirstName (Person "" "" 0)           @?= (Person "" "" 0)


example_tree1 = Node 
    (Node Nil 1 (Node Nil 2 Nil))
    3
    (Node (Node Nil 4 Nil) 5 Nil)

example_tree2 = Node
    (Node Nil 1 Nil)
    2
    (Node
        (Node (Node Nil 3 Nil) 4 Nil)
        5
        (Node Nil 6 Nil)
    )

example_tree3 = Node Nil 100 Nil

unit_treeSum :: Assertion
unit_treeSum = do
    treeSum Nil           @?= 0
    treeSum example_tree1 @?= 15
    treeSum example_tree2 @?= 21
    treeSum example_tree3 @?= 100

unit_treeHeight :: Assertion
unit_treeHeight = do
    treeHeight Nil           @?= 0
    treeHeight example_tree1 @?= 3
    treeHeight example_tree2 @?= 4
    treeHeight example_tree3 @?= 1

unit_EqTree :: Assertion
unit_EqTree = do
    (Nil           == (Nil::(Tree Integer))) @?= True
    (Nil           == example_tree1)         @?= False
    (Nil           == example_tree2)         @?= False
    (Nil           == example_tree3)         @?= False

    (example_tree1 == Nil)                   @?= False
    (example_tree1 == example_tree1)         @?= True
    (example_tree1 == example_tree2)         @?= False
    (example_tree1 == example_tree3)         @?= False

    (example_tree2 == Nil)                   @?= False
    (example_tree2 == example_tree1)         @?= False
    (example_tree2 == example_tree2)         @?= True
    (example_tree2 == example_tree3)         @?= False

    (example_tree3 == Nil)                   @?= False
    (example_tree3 == example_tree1)         @?= False
    (example_tree3 == example_tree2)         @?= False
    (example_tree3 == example_tree3)         @?= True

unit_FoldableTree :: Assertion
unit_FoldableTree = do
    to_list     Nil           @?= ([]::[Integer])
    to_list     example_tree1 @?= [1, 2, 3, 4, 5]
    to_list     example_tree2 @?= [1, 2, 3, 4, 5, 6]
    to_list     example_tree3 @?= [100]
    foldr (-) 1 example_tree2 @?= -2
  where to_list = foldr (:) []

unit_FoldablePreorderTree :: Assertion
unit_FoldablePreorderTree = do
    to_list     (PreO Nil)           @?= ([]::[Integer])
    to_list     (PreO example_tree1) @?= [3, 1, 2, 5, 4]
    to_list     (PreO example_tree2) @?= [2, 1, 5, 4, 3, 6]
    to_list     (PreO example_tree3) @?= [100]
    foldr (-) 2 (PreO example_tree2) @?= 1
  where to_list = foldr (:) []

unit_FoldablePostorderTree :: Assertion
unit_FoldablePostorderTree = do
    to_list     (PostO Nil)           @?= ([]::[Integer])
    to_list     (PostO example_tree1) @?= [2, 1, 4, 5, 3]
    to_list     (PostO example_tree2) @?= [1, 3, 4, 6, 5, 2]
    to_list     (PostO example_tree3) @?= [100]
    foldr (-) 2 (PostO example_tree2) @?= 1
  where to_list = foldr (:) []

unit_FoldableLevelorderTree :: Assertion
unit_FoldableLevelorderTree = do
    to_list     (LevelO Nil)           @?= ([]::[Integer])
    to_list     (LevelO example_tree1) @?= [3, 1, 5, 2, 4]
    to_list     (LevelO example_tree2) @?= [2, 1, 5, 4, 6, 3]
    to_list     (LevelO example_tree3) @?= [100]
    foldr (-) 1 (LevelO example_tree2) @?= 6
  where to_list = foldr (:) []

unit_treeSum' :: Assertion
unit_treeSum' = do
    treeSum' Nil           @?= 0
    treeSum' example_tree1 @?= 15
    treeSum' example_tree2 @?= 21
    treeSum' example_tree3 @?= 100


example_list1 = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Empty))))
example_list2 = Cons 1 example_list2
example_list3 = Cons 2 example_list3

unit_EqMyList :: Assertion
unit_EqMyList = do
    (Empty         == (Empty::(MyList Integer))) @?= True
    (Empty         == example_list1)             @?= False
    (Empty         == example_list2)             @?= False
    (Empty         == example_list3)             @?= False

    (example_list1 == Empty)                     @?= False
    (example_list1 == example_list1)             @?= True
    (example_list1 == example_list2)             @?= False
    (example_list1 == example_list3)             @?= False

    (example_list2 == Empty)                     @?= False
    (example_list2 == example_list1)             @?= False
    (example_list2 == example_list3)             @?= False

    (example_list3 == Empty)                     @?= False
    (example_list3 == example_list1)             @?= False
    (example_list3 == example_list2)             @?= False

unit_OrdMyList :: Assertion
unit_OrdMyList = do
    (Empty         <= (Empty::(MyList Integer))) @?= True
    (Empty         <= example_list1)             @?= True
    (Empty         <= example_list2)             @?= True
    (Empty         <= example_list3)             @?= True

    (example_list1 <= Empty)                     @?= False
    (example_list1 <= example_list1)             @?= True
    (example_list1 <= example_list2)             @?= False
    (example_list1 <= example_list3)             @?= True

    (example_list2 <= Empty)                     @?= False
    (example_list2 <= example_list1)             @?= True
    (example_list2 <= example_list3)             @?= True

    (example_list3 <= Empty)                     @?= False
    (example_list3 <= example_list1)             @?= False
    (example_list3 <= example_list2)             @?= False

unit_FoldableMyList :: Assertion
unit_FoldableMyList = do
    foldr (+)   0 Empty         @?= 0
    foldr (+)   0 example_list1 @?= 15
    foldr (-)   1 example_list1 @?= 2
    foldr const 0 example_list2 @?= 1

unit_FunctorMyList :: Assertion
unit_FunctorMyList = do
    fmap id          Empty         @?= (Empty::(MyList Integer))
    fmap id          example_list1 @?= example_list1
    fmap (const 'A') example_list1 @?= Cons 'A' (Cons 'A' (Cons 'A' (Cons 'A' (Cons 'A' Empty))))
    take' 100 (fmap (+1) example_list2) @?= take' 100 example_list3
  where take' _ Empty       = Empty
        take' 0 _           = Empty
        take' n (Cons x xs) = Cons x $ take' (n - 1) xs


unit_sum2D :: Assertion
unit_sum2D = do
    sum2D Empty                                      @?= 0
    sum2D (fmap (const example_list1) example_list1) @?= 75

module Test.Tasks where

import Tasks (head', take', tail', drop', filter', zip', map', foldr', foldl', concat')

import Test.Tasty.HUnit (Assertion, (@?=))

unit_head' :: Assertion
unit_head' = do
    head' [1, 2, 3] @?= 1
    head' [239]     @?= 239
    head' [5, 7..]  @?= 5

unit_take' :: Assertion
unit_take' = do
    take' 2 [1, 2, 3] @?= [1, 2]
    take' 5 [1, 2, 3] @?= [1, 2, 3]
    take' 1 [239]     @?= [239]
    take' 0 [239]     @?= []
    take' 0 []        @?= ([]::[Int])
    take' 3 [5, 7..]  @?= [5, 7, 9]

unit_tail' :: Assertion
unit_tail' = do
    tail' [1, 2, 3]             @?= [2, 3]
    tail' [1]                   @?= []
    tail' []                    @?= ([]::[Int])
    take' 100 (tail' [5, 7..])  @?= take' 100 [7, 9..]

unit_drop' :: Assertion
unit_drop' = do
    drop' 0 [1, 2, 3]        @?= [1, 2, 3]
    drop' 1 [1, 2, 3]        @?= [2, 3]
    drop' 2 [1, 2, 3]        @?= [3]
    drop' 3 [1, 2, 3]        @?= []
    drop' 4 [1, 2, 3]        @?= []
    drop' 1 [1]              @?= []
    drop' 5 []               @?= ([]::[Int])
    take' 100 (drop 5 [1..]) @?= take' 100 [6..]

unit_filter' :: Assertion
unit_filter' = do
    filter' odd           [1..10] @?= [1, 3..9]
    filter' (const True)  [1..10] @?= [1..10]
    filter' (const False) [1..10] @?= []
    filter' odd           []      @?= []

unit_zip' :: Assertion
unit_zip' = do
    zip' [1, 2, 3] [4, 5, 6] @?= [(1, 4), (2, 5), (3, 6)]
    zip' [1, 2, 3] [4, 5]    @?= [(1, 4), (2, 5)]
    zip' [1, 2]    [4, 5, 6] @?= [(1, 4), (2, 5)]
    zip' [1, 2, 3] []        @?= ([]::[(Int, Char)])
    zip' []        []        @?= ([]::[(Int, Char)])
    zip' [1, 2, 3] [4..]     @?= [(1, 4), (2, 5), (3, 6)]

unit_map' :: Assertion
unit_map' = do
    map' (^2) [1, 2, 3]         @?= [1, 4, 9]
    map' id   [1, 2, 3]         @?= [1, 2, 3]
    map' (^2) [3]               @?= [9]
    map' (^2) []                @?= []
    map' odd  [1, 2, 3]         @?= [True, False, True]
    take' 100 (map' (^2) [1..]) @?= take' 100 [x^2 | x <- [1..]]

unit_foldr' :: Assertion
unit_foldr' = do
    foldr' const 0 [1..]   @?= 1
    foldr' (+)   0 [1..10] @?= 55
    foldr' (-)   0 [1..10] @?= -5
    foldr' (*)   3 []      @?= 3
    foldr' (*)   3 []      @?= 3

unit_foldl' :: Assertion
unit_foldl' = do
    foldl' (+) 0 [1..10] @?= 55
    foldl' (-) 0 [1..10] @?= -55
    foldl' (*) 3 []      @?= 3

unit_concat' :: Assertion
unit_concat' = do
    concat' [1, 2, 3] [4, 5, 6]         @?= [1..6]
    concat' []        [4, 5, 6]         @?= [4, 5, 6]
    concat' [1, 2, 3] []                @?= [1, 2, 3]
    concat' []        []                @?= ([]::[Int])
    take' 100 (concat' [1, 2, 3] [4..]) @?= take' 100 [1..]

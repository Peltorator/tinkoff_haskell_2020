module Test.Tasks2 where

import Tasks2 (sum_of_lists, n'th_prime, tails', inits', reverse', reverse'')

import Test.Tasty.HUnit (Assertion, (@?=))

unit_sum_of_lists :: Assertion
unit_sum_of_lists = do
    sum_of_lists [[1, 2, 3, 4], [10, 20, 30], [100, 200, 300, 400, 500]] @?= [111, 222, 333]
    sum_of_lists [[1, 2, 3, 4], [10, 20, 30], []]                        @?= []
    sum_of_lists [[1, 2, 3, 4, 5]]                                       @?= [1, 2, 3, 4, 5]
    sum_of_lists [[]]                                                    @?= []
    sum_of_lists [[1, 2, 3], [1..]]                                      @?= [2, 4, 6]
    take 100 (sum_of_lists [[1..], [1..]])                               @?= [2, 4..200]
    sum_of_lists (([]::[Int]):zeros_zeros)                               @?= []
  where zeros = 0:zeros
        zeros_zeros = zeros:zeros_zeros


unit_n'th_prime :: Assertion
unit_n'th_prime = do
    n'th_prime 0     @?= 2
    n'th_prime 1     @?= 3
    n'th_prime 2     @?= 5
    n'th_prime 3     @?= 7
    n'th_prime 4     @?= 11
    n'th_prime 5     @?= 13
    n'th_prime 10000 @?= 104743

unit_tails' :: Assertion
unit_tails' = do
    tails' [1, 2, 3]                      @?= [[1, 2, 3], [2, 3], [3], []]
    tails' [1]                            @?= [[1], []]
    tails' []                             @?= [[]::[Int]]
    fmap (take 3) (take 3 (tails' [1..])) @?= [[1, 2, 3], [2, 3, 4], [3, 4, 5]]

unit_inits' :: Assertion
unit_inits' = do
    inits' [1, 2, 3]      @?= [[], [1], [1, 2], [1, 2, 3]]
    inits' [1]            @?= [[], [1]]
    inits' []             @?= [[]::[Int]]
    take 3 (inits' [1..]) @?= [[], [1], [1, 2]]

unit_reverse' :: Assertion
unit_reverse' = do
    reverse' [1, 2, 3]  @?= [3, 2, 1]
    reverse' [1]        @?= [1]
    reverse' []         @?= ([]::[Int])
    reverse' [1..5000]  @?= [5000, 4999..1]

unit_reverse'' :: Assertion
unit_reverse'' = do
    reverse'' [1, 2, 3]   @?= [3, 2, 1]
    reverse'' [1]         @?= [1]
    reverse'' []          @?= ([]::[Int])
    reverse'' [1..100000] @?= [100000, 99999..1]    

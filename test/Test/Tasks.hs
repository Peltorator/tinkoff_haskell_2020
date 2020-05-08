module Test.Tasks where

import Tasks (head')

import Test.Tasty.HUnit (Assertion, (@?=))

unit_head :: Assertion
unit_head = do
    head' [1, 2, 3] @?= 1



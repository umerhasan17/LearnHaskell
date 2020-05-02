{-# LANGUAGE FlexibleInstances #-}

import Data.Array

class Stack stack where
    empty :: stack
    push :: Int -> stack -> stack
    pop :: stack -> stack
    peek :: stack -> Maybe Int
    -- default implementation
    look :: Int -> stack -> Maybe Int
    look 0 xs = peek xs
    look i xs = look (i-1) (pop xs)

instance Stack [Int] where
    empty = []
    push x xs = (x:xs)
    pop [] = []
    pop (x:xs) = xs
    peek [] = Nothing
    peek (x:xs) = Just x

-- ii) T(n) = 1 + T(n-1), T(0) = 1 hence O(n) = n + 1

data StackArray = StackArray (Array Int Int) Int Int

instance Stack StackArray where
    empty = fromList []   -- REMEMBER TO RETURN STACK ARRAY StackArray (fromList []) 0 0 
    push x s@(StackArray arr l c)
        | l == c    = s -- (StackArray (fromList (x:elems arr)) (l+1) (2*c))
        | otherwise = (StackArray (modify arr l x) (l+1) c)
    pop (StackArray arr l c)
        | l == 0    = empty -- just return input instead (StackArray arr l c)
        | otherwise = (StackArray arr (l-1) c)
    peek (StackArray arr l c)
        | l == 0    = Nothing
        | otherwise = Just (arr ! (l-1))
    look i (StackArray arr l c)
        | i < l && i >= 0 = Just (arr ! i)
        | otherwise       = Nothing

modify = undefined
fromList = undefined
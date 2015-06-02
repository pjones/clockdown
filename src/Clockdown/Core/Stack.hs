{-

This file is part of the package clockdown. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/clockdown/LICENSE. No part of
the clockdown package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Clockdown.Core.Stack
       ( Stack
       , stack
       , push
       , pop
       , head
       , rotateLeft
       , rotateRight
       ) where

--------------------------------------------------------------------------------
import Prelude hiding (head)

--------------------------------------------------------------------------------
-- | A stack that can't be empty.
data Stack a = Node a | List a (Stack a)

--------------------------------------------------------------------------------
instance Functor Stack where
  fmap f (Node x)    = Node (f x)
  fmap f (List x xs) = List (f x) (fmap f xs)

--------------------------------------------------------------------------------
stack :: a -> Stack a
stack = Node

--------------------------------------------------------------------------------
push :: a -> Stack a -> Stack a
push x (Node y)    = List x (Node y)
push x (List y ys) = List x (List y ys)

--------------------------------------------------------------------------------
pop :: Stack a -> (a, Stack a)
pop (Node x)    = (x, Node x)
pop (List x xs) = (x, xs)

--------------------------------------------------------------------------------
rpush :: a -> Stack a -> Stack a
rpush x (Node y)    = List y (Node x)
rpush x (List y ys) = List y (rpush x ys)

--------------------------------------------------------------------------------
rpop :: Stack a -> (a, Stack a)
rpop (Node x)          = (x, Node x)
rpop (List x (Node y)) = (y, Node x)
rpop (List x xs)       = let (y, ys) = rpop xs in (y, List x ys)

--------------------------------------------------------------------------------
head :: Stack a -> a
head (Node x)   = x
head (List x _) = x

--------------------------------------------------------------------------------
rotateLeft :: Stack a -> Stack a
rotateLeft (Node x)          = Node x
rotateLeft (List x (Node y)) = List y (Node x)
rotateLeft (List x xs)       = List (head xs) (rpush x . snd $ pop xs)

--------------------------------------------------------------------------------
rotateRight :: Stack a -> Stack a
rotateRight (Node x)          = Node x
rotateRight (List x (Node y)) = List y (Node x)
rotateRight (List x xs)       = let (z, zs) = rpop xs in List z (List x zs)

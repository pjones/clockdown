{-

This file is part of the package clockdown. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/clockdown/LICENSE. No part of
the clockdown package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | A stack type and functions to manipulate it.
module Clockdown.Core.Stack
       ( Stack
       , stack
       , withFocused
       , push
       , pop
       , focus
       , focusLeft
       , focusRight
       ) where

--------------------------------------------------------------------------------
-- | A stack that can't be empty and has a focused element.
data Stack a = Stack [a] a [a]

--------------------------------------------------------------------------------
instance Functor Stack where
  fmap f (Stack a b c) = Stack (fmap f a) (f b) (fmap f c)

--------------------------------------------------------------------------------
-- | Create a new 'Stack'.
stack :: a -> Stack a
stack x = Stack [] x []

--------------------------------------------------------------------------------
-- | Apply a function to the focused element.
withFocused :: Stack a -> (a -> a) -> Stack a
withFocused (Stack a b c) f = Stack a (f b) c

--------------------------------------------------------------------------------
-- | Push an item onto the end of the stack and focus it.
push :: a -> Stack a -> Stack a
push x (Stack a b c) = Stack (a ++ [b] ++ c) x []

--------------------------------------------------------------------------------
-- | Pop the focused item off the stack and focus the next available item.
pop :: Stack a -> Stack a
pop (Stack [] b [])     = Stack [] b []
pop (Stack as _ (c:cs)) = Stack as c cs
pop (Stack a@(_:_) _ c) = Stack (init a) (last a) c

--------------------------------------------------------------------------------
-- | Get the focused element.
focus :: Stack a -> a
focus (Stack _ b _) = b

--------------------------------------------------------------------------------
-- | Change the focus to the next previous element.
focusLeft :: Stack a -> Stack a
focusLeft (Stack [] b [])     = Stack [] b []
focusLeft (Stack a@(_:_) b c) = Stack (init a) (last a) (b:c)
focusLeft (Stack a b c@(_:_)) = Stack (a ++ [b] ++ init c) (last c) []

--------------------------------------------------------------------------------
-- | Change the focus to the previous element.
focusRight :: Stack a -> Stack a
focusRight (Stack [] b [])     = Stack [] b []
focusRight (Stack as b (c:cs)) = Stack (as ++ [b]) c cs
focusRight (Stack (a:as) b c)  = Stack [] a (as ++ [b] ++ c)

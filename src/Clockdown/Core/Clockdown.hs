{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-

This file is part of the package clockdown. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/clockdown/LICENSE. No part of
the clockdown package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Clockdown.Core.Clockdown
       ( Clockdown
       , ask
       , get
       , put
       , liftIO
       , runClockdown
       ) where

--------------------------------------------------------------------------------
import Control.Monad.RWS

--------------------------------------------------------------------------------
import Clockdown.Core.Stack
import Clockdown.Core.Window

--------------------------------------------------------------------------------
newtype Clockdown r m a = Clockdown {unC :: RWST r () (Stack Window) m a}
                        deriving ( Functor, Applicative, Monad, MonadIO
                                 , MonadReader r, MonadState (Stack Window)
                                 )

--------------------------------------------------------------------------------
runClockdown :: (Monad m) => r -> Stack Window -> Clockdown r m a -> m a
runClockdown r s c = do (a, _, _) <- runRWST (unC c) r s
                        return a

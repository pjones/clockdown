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
       , asks
       , get
       , gets
       , put
       , modify
       , config
       , private
       , liftIO
       , quit
       , runClockdown
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Control.Monad.RWS
import Control.Monad.Trans.Maybe

--------------------------------------------------------------------------------
-- Local imports:
import Clockdown.Core.Config
import Clockdown.Core.Stack
import Clockdown.Core.Window

--------------------------------------------------------------------------------
data Env r = Env
  { config  :: Config
  , private :: r
  }

--------------------------------------------------------------------------------
newtype Clockdown r m a =
  Clockdown {unC :: RWST (Env r) () (Stack Window) (MaybeT m) a}
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadReader (Env r), MonadState (Stack Window)
           )

--------------------------------------------------------------------------------
quit :: (Monad m) => Clockdown r m a
quit = Clockdown $ lift (MaybeT $ return Nothing)

--------------------------------------------------------------------------------
runClockdown :: (Monad m)
             => r
             -> Config
             -> Stack Window
             -> Clockdown r m a
             -> m (Maybe a)

runClockdown r cfg s c =
  do result <- runMaybeT $ runRWST (unC c) env s
     case result of
       Nothing        -> return Nothing
       Just (a, _, _) -> return (Just a)

  where
    env = Env { config  = cfg
              , private = r
              }

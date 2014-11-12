{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Monad.Bracket (
  BracketT,
  MonadBracket(..),
  runBracketT
  ) where

import Monad.Cont
import Monad.Try

import Control.Monad.Trans.Cont (ContT(ContT), runContT)
import Control.Monad.Trans.Class

import Control.Monad.Lift

#if MIN_VERSION_mmorph(1, 0, 1)
import           Control.Monad.Trans.Compose (ComposeT (ComposeT))
#endif

type BracketT m a = (MonadTry m, Monad m) => ContT () m a

class (Monad m) => MonadBracket m where
  type BracketInner m :: * -> *
  bracketC :: (Monad n, n ~ BracketInner m) => n a -> (a -> n b) -> m a
  bracketC_ :: (Monad n, n ~ BracketInner m ) => n a -> n b -> m a
  bracketC_ start final = bracketC start $ const final

#if MIN_VERSION_mmorph(1, 0, 1)
instance (MonadBracket (f (g m)),
          BracketInner (f (g m)) ~ BracketInner (ComposeT f g m))
         => MonadBracket (ComposeT f g m) where
  bracketC start final = ComposeT (bracketC start final)
#endif

class (Monad m, Monad n) => MonadBracket' m n where
  bracketC' :: n a -> (a -> n b) -> m a

instance (MonadTry n) => MonadBracket' (ContT () n) n where
  bracketC' start final = ContT $ bracket start final

instance (MonadBracket' m n, n ~ BracketInner m) => MonadBracket m where
  bracketC = bracketC'

runBracketT :: (MonadTry m) => BracketT m a -> m ()
runBracketT b = runContT b (\_ -> return ())

{-# LANGUAGE TupleSections #-}
module SoOSiM.CLI.Util
  ( mapAccumLM
  , mapAccumRM
  )
where

import Control.Applicative (Applicative(..))
import Control.Monad.State (StateT(..))
import Data.Traversable    (Traversable(..))

mapAccumLM ::
  (Monad m, Functor m, Traversable t)
  => (a -> b -> m (c,a))
  -> a
  -> t b
  -> m (t c, a)
mapAccumLM f s t = runStateT (traverse (StateT . flip f) t) s

newtype StateTR s m a = StateTR { runStateTR :: s -> m (a, s) }

instance Functor m => Functor (StateTR s m) where
    fmap f m = StateTR $ \s ->
      fmap (\(~(a,s')) -> (f a, s')) $ runStateTR m s

instance (Functor m, Monad m) => Applicative (StateTR s m) where
  pure x = StateTR (return . (x,))
  StateTR kf <*> StateTR kv = StateTR $ \s -> do
    ~(v,s')  <- kv s
    ~(f,s'') <- kf s'
    return (f v, s'')

mapAccumRM ::
  (Monad m, Functor m, Traversable t)
  => (a -> b -> m (c,a))
  -> a
  -> t b
  -> m (t c, a)
mapAccumRM f s t = runStateTR (traverse (StateTR . flip f) t) s

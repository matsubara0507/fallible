{-# LANGUAGE TypeFamilies #-}

module Data.Fallible where

import           Control.Monad.Trans.Cont

class Fallible f where
  type Failure f :: *
  tryFallible :: f a -> Either (Failure f) a

instance Fallible Maybe where
  type Failure Maybe = ()
  tryFallible = maybe (Left ()) Right

instance Fallible (Either e) where
  type Failure (Either e) = e
  tryFallible = id

(??=) :: (Applicative f, Fallible t) => t a -> (Failure t -> f a) -> f a
t ??= k = either k pure $ tryFallible t
{-# INLINE (??=) #-}
infixl 1 ??=

(???) :: (Applicative f, Fallible t) => t a -> f a -> f a
t ??? k = t ??= const k
{-# INLINE (???) #-}
infixl 1 ???

(!?=) :: (Monad m, Fallible t) => m (t a) -> (Failure t -> m a) -> m a
t !?= k = t >>= (??=k)
{-# INLINE (!?=) #-}
infixl 1 !?=

catchFailure :: (Monad m, Fallible t) => m (t a) -> (Failure t -> m a) -> m a
catchFailure = (!?=)
{-# INLINE catchFailure #-}

(!??) :: (Monad m, Fallible t) => m (t a) -> m a -> m a
t !?? k = t >>= (???k)
{-# INLINE (!??) #-}
infixl 1 !??

catchFailure_ :: (Monad m, Fallible t) => m (t a) -> m a -> m a
catchFailure_ = (!??)
{-# INLINE catchFailure_ #-}

exit :: m r -> ContT r m a
exit = ContT . const
{-# INLINE exit #-}

exitA :: Applicative m => r -> ContT r m a
exitA = exit . pure
{-# INLINE exitA #-}

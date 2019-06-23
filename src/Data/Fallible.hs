{-# LANGUAGE TypeFamilies #-}

module Data.Fallible
  ( Fallible (..)
  , (??=)
  , (???)
  , (!?=)
  , (!??)
  , catchFailure
  , catchFailure_
  , exit
  , exitA
  , module Cont
  , lift
  ) where

import           Control.Applicative
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Cont  as Cont
import           Data.Functor.Identity
import           Data.Proxy
import           Data.Void

-- | Types that may contain failures.
class Applicative f => Fallible f where
  type Failure f :: *

  -- | Get a success or a failure.
  --
  -- @'tryFallible' (pure a) ≡ Right a@
  tryFallible :: f a -> Either (Failure f) a

instance Fallible Identity where
  type Failure Identity = Void
  tryFallible = Right . runIdentity

instance Monoid e => Fallible (Const e) where
  type Failure (Const e) = e
  tryFallible = Left . getConst

instance Fallible Proxy where
  type Failure Proxy = ()
  tryFallible _ = Left ()

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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  (<$>) :: (a -> b) -> Compose f g a -> Compose f g b
  (<$>) ab (Compose fga) =
    -- let fgb = (ab <$>) <$> fga
    -- in Compose fgb
    Compose ((ab <$>) <$> fga)
    

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure :: a -> Compose f g a
  pure =
    Compose . pure . pure
-- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (<*>) (Compose fgab) (Compose fga) =
    -- Compose ((<*>) <$> fgab <*> fga)
    Compose (lift2 (<*>) fgab fga)

-- NOTE:
-- Monads don't compose. This is the motivation for monad transformers.
-- which allow composition of monads, when 'g' is fixed

-- instance (Monad f, Monad g) =>
--   Monad (Compose f g) where
-- -- Implement the (=<<) function for a Monad instance for Compose
--   (=<<) =
--     error "todo: Course.Compose (<<=)#instance (Compose f g)"

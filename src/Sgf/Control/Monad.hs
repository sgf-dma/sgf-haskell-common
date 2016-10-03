
module Sgf.Control.Monad
    ( when'
    )
  where


when' :: (Monad m, Monoid a) => Bool -> m a -> m a
when' b mx
  | b               = mx
  | otherwise       = return mempty

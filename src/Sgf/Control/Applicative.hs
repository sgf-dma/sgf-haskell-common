
module Sgf.Control.Applicative
    ( (<.>)
    )
  where

import Control.Applicative


(<.>) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
(<.>)               = liftA2 (.)


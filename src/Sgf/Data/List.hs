
module Sgf.Data.List
    ( insertUniq
    , mapWhen
    , mapWhenM
    , findM
    , ReadM
    , liftRead
    , readsPrecM
    , readLexs
    , readLexsM
    , anyP
    , when'
    , unless'
    , riseElems
    )
  where

import Control.Applicative
import Control.Monad.State

insertUniq :: Eq a => a -> [a] -> [a]
insertUniq y xs
  | y `elem` xs     = xs
  | otherwise       = y : xs

mapWhen :: Eq a => (a -> Bool) -> (a -> a) -> [a] -> [a]
mapWhen p f         = map (\x -> if p x then f x else x)

mapWhenM :: Eq a => Monad m => (a -> Bool) -> (a -> m a) -> [a] -> m [a]
mapWhenM p f        = mapM (\x -> if p x then f x else return x)

-- Search a list for element, where predicate returns True, and does *not* run
-- monad actions on remaining elements (after match has found). I.e. just like
-- regular `find` does, but with monadic predicate.
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ []          = return Nothing
findM f (x : xs)    = f x >>= \b -> if b then return (Just x) else findM f xs


-- Store remaining string in State monad.
type ReadM a        = StateT String [] a

-- Lift ReadS to ReadM .
liftRead :: Read a => ReadS a -> ReadM a
liftRead h = do
    r0 <- get
    (x, r1) <- lift (h r0)
    put r1
    return x

readsPrecM :: Read a => Int -> ReadM a
readsPrecM      = liftRead . readsPrec

-- Try to read specified lexems in turn and throw them away (thus, returning
-- remaining part of string).
readLexs :: [String] -> ReadS ()
readLexs [] ys       = [((), ys)]
readLexs (x : xs) ys = do
                        (z, zs) <- lex ys
                        if x == z then readLexs xs zs else []

readLexsM :: [String] -> ReadM ()
readLexsM           = liftRead . readLexs


anyP :: [a -> Bool] -> a -> Bool
anyP fs             = or . sequence fs

when' :: (Monoid b, Monad m) => Bool -> m b -> m b
when' p mx
 | p                = mx
 | otherwise        = return mempty

unless' :: (Monoid b, Monad m) => Bool -> m b -> m b
unless' p mx
 | not p            = mx
 | otherwise        = return mempty

-- Move all elements, matching predicate, to the head of list.
riseElems :: (a -> Bool) -> [a] -> [a]
--riseElems p xs      = filter p xs ++ filter (not . p) xs
riseElems p         = liftA2 (++) (filter p) (filter (not . p))


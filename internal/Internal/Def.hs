module Internal.Def (
    Pos,
    RuleMonad(..),
    getVal,
    getPos
) where

import Data.List (union)

-- | A position is a 3D coordinate in the world
type Pos = (Int, Int, Int)

data RuleMonad m = RuleMonad m [Pos]

getVal :: RuleMonad a -> a
getVal (RuleMonad a _) = a

getPos :: RuleMonad a -> [Pos]
getPos (RuleMonad _ pos) = pos

instance Functor RuleMonad where
    fmap :: (a -> b) -> RuleMonad a -> RuleMonad b
    fmap f (RuleMonad a pos) = RuleMonad (f a) pos

instance Applicative RuleMonad where
    pure :: a -> RuleMonad a
    pure a = RuleMonad a []
    (<*>) :: RuleMonad (a -> b) -> RuleMonad a -> RuleMonad b
    (RuleMonad f pos1) <*> (RuleMonad v pos2) = RuleMonad (f v) (pos1 `union` pos2) 

instance Monad RuleMonad where
    return :: a -> RuleMonad a
    return = pure
    (>>=) :: RuleMonad a -> (a -> RuleMonad b) -> RuleMonad b
    RuleMonad v1 pos1 >>= f = let RuleMonad v2 pos2 = f v1 in RuleMonad v2 (pos1 `union` pos2)

instance Semigroup a => Semigroup (RuleMonad a) where
    (<>) :: RuleMonad a -> RuleMonad a -> RuleMonad a
    RuleMonad v1 pos1 <> RuleMonad v2 pos2 = RuleMonad (v1 <> v2) (pos1 `union` pos2)

instance Monoid a => Monoid (RuleMonad a) where
    mempty :: RuleMonad a
    mempty = pure mempty
    mappend :: RuleMonad a -> RuleMonad a -> RuleMonad a
    mappend = (<>)

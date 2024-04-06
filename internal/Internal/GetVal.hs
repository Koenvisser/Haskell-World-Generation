module Internal.GetVal where

import Def

getVal :: RuleMonad a -> a
getVal (RuleMonad a _) = a

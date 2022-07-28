module SKI (S (..), K (..)) where

import Data.Kind

type CPS :: Type -> Type
type CPS a = (a -> Type) -> Type

type MkCPS :: a -> CPS a
newtype MkCPS a k = MkCPS (k a)

type G :: (a -> b) -> (b -> Type) -> a -> Type
newtype G f k x = G (k (f x))

type F :: CPS a -> (b -> Type) -> (a -> b) -> Type
newtype F x k f = F (x (G f k))

type App :: CPS (a -> b) -> CPS a -> CPS b
newtype App f x k = App (f (F x k))

type UnCPS :: (CPS a -> CPS b) -> a -> CPS b
newtype UnCPS f x k = UnCPS (f (MkCPS x) k)

type Id' :: (a -> Type) -> a -> Type
newtype Id' f x = Id' (f x)

type Id :: CPS a -> CPS a
newtype Id x k = Id (x (Id' k))

newtype Join' k x = Join' (x k)
type Join :: CPS (CPS a) -> CPS a
newtype Join x k = Join (x (Join' k))

-- newtype Comp :: (b -> c) -> (a -> b) -> a ->

type (-->) a b = CPS (CPS a -> CPS b)

-- <*>
type S :: forall a b c. (a --> b --> c) --> (a --> b) --> a --> c
type family S where

type K :: forall a b. a --> b --> a
type family K where

{-
-- FIXME possibly Reduce
newtype S f g x k = S {unS :: k (f x (g x))}

-- pure
type K :: forall a b. a -> b -> CPS a
-- FIXME possibly Reduce
newtype K x y k = K {unK :: k x}

-}
type K' :: a --> () --> a
type K' = K

-- type I :: forall a. a --> a
type I = S K K'

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
-- broken
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

-- module Single (Some(..), Forall (..), Single, unSingle, single, known', known, know, Known, HasSingle (..)) where
module Single (Single (..), single, UnSingle, MkSingle, Known, known, known', know, scon, smatch, Matcher (..)) where

import Unsafe.Coerce (unsafeCoerce)

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import Data.SOP (I (I), NP (Nil, (:*)))
import GHC.Generics (Generic)
import GHC.Prim (Proxy#)
import GHC.TypeLits (Nat, type (-))
import Generics.SOP.GGP (GCode)

-- Core types

newtype Single (x :: a) = Single {unSingle :: a}
  deriving newtype (Show)

single :: a -> (Single (x :: a) -> b) -> b
single x f = f (Single x)

type family MkSingle :: forall (x :: a) -> Single x where

type UnSingle :: forall (a :: Type) (x :: a). Single x -> a
type family UnSingle (sx :: Single (x :: a)) :: a where
  UnSingle @_ @x _ = x

class KnownInternal (x :: a) where
  knownImpl :: Single x

class KnownInternal x => Known x
instance KnownInternal x => Known x

known :: Known x => Proxy x -> Single x
known _ = knownImpl

known' :: Known x => Proxy# x -> Single x
known' _ = knownImpl

newtype Helper (x :: a) b = Helper (Known x => b)

know :: forall (a :: Type) (b :: Type) (x :: a). Single x -> (Known x => b) -> b
know x f = unsafeCoerce (Helper f :: Helper x b) x

-- Dealing with user defined types

type Coerce :: forall (a :: Type) (b :: Type). a -> b
type family Coerce (x :: a) :: b where
  Coerce x = x

type family IndexList (i :: Nat) (xs :: [a]) :: a where
  IndexList 0 (x ': _) = x
  IndexList n (_ ': xs) = IndexList (n - 1) xs

type family CanIndexList (i :: Nat) (xs :: [a]) :: Bool where
  CanIndexList _ '[] = 'False
  CanIndexList 0 (_ ': _) = 'True
  CanIndexList n (_ ': xs) = CanIndexList (n - 1) xs

type IndexNP :: forall (xst :: [Type]). forall (i :: Nat) -> NP I xst -> IndexList i xst
type family IndexNP (i :: Nat) (xs :: NP I xst) :: IndexList i xst where
  IndexNP 0 ( 'I x ':* _) = x
  IndexNP n (_ ':* xs) = Coerce (IndexNP (n - 1) xs)

type GiveForall ::
  forall (a :: [Type]) ->
  ( NP
      I
      '[ IndexList 0 a
       , IndexList 1 a
       , IndexList 2 a
       , IndexList 3 a
       , IndexList 4 a
       , IndexList 5 a
       , IndexList 6 a
       , IndexList 7 a
       , IndexList 8 a
       , IndexList 9 a
       , IndexList 10 a
       , IndexList 11 a
       , IndexList 12 a
       , IndexList 13 a
       , IndexList 14 a
       , IndexList 15 a
       , IndexList 16 a
       , IndexList 17 a
       , IndexList 18 a
       , IndexList 19 a
       , IndexList 20 a
       , IndexList 21 a
       , IndexList 22 a
       , IndexList 23 a
       ] ->
    Type
  ) ->
  Type
type GiveForall a f =
  forall a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23.
  f
    ( 'I a0
        ':* 'I a1
        ':* 'I a2
        ':* 'I a3
        ':* 'I a4
        ':* 'I a5
        ':* 'I a6
        ':* 'I a7
        ':* 'I a8
        ':* 'I a9
        ':* 'I a10
        ':* 'I a11
        ':* 'I a12
        ':* 'I a13
        ':* 'I a14
        ':* 'I a15
        ':* 'I a16
        ':* 'I a17
        ':* 'I a18
        ':* 'I a19
        ':* 'I a20
        ':* 'I a21
        ':* 'I a22
        ':* 'I a23
        ':* 'Nil
    )

type family ConstructorOf (t :: Type) (args :: [Type]) :: Type where
  ConstructorOf t '[] = t
  ConstructorOf t (x ': xs) = x -> ConstructorOf t xs

type ConstructorType' :: Type -> Type
type family ConstructorType' (ct :: Type) :: Type where
  ConstructorType' (_ -> b) = ConstructorType' b
  ConstructorType' a = a

type ConstructorType :: forall (ct :: Type). ct -> Type
type family ConstructorType (c :: ct) :: Type where
  ConstructorType @a _ = ConstructorType' a

type ConstructorArgs' :: Type -> [Type]
type family ConstructorArgs' (ct :: Type) :: [Type] where
  ConstructorArgs' (a -> b) = a ': ConstructorArgs' b
  ConstructorArgs' _ = '[]

type ValidateConstructor' :: forall (cts :: [Type]) (ct :: Type). NP I cts -> ct -> Constraint
type family ValidateConstructor' (cs :: NP I cts) (c :: ct) :: Constraint where
  ValidateConstructor' ( 'I c ':* cs) c = ()
  ValidateConstructor' (_ ':* cs) c = ValidateConstructor' cs c

type family ValidateConstructor (c :: ct) :: Constraint where
  ValidateConstructor c = ValidateConstructor' (GetConstructors (ConstructorType c)) c

type family ConstructorsOf (t :: Type) (argss :: [[Type]]) :: [Type] where
  ConstructorsOf _ '[] = '[]
  ConstructorsOf t (x ': xs) = ConstructorOf t x ': ConstructorsOf t xs

-- Should be `type family Constructors (t :: Type) :: NP I (ConstructorsOf t (GCode t))`.
-- The above definition doesn't permit you to define any instances for some reason,
-- seems like GHC doesn't reduce the types enough, and I don't know how to force it to.
-- Use `GetConstructors` to actually retrieve the instance.
type family UnsafeDeclConstructors (t :: Type) :: Maybe (NP I constructors)

type family FromJust (x :: Maybe a) :: a where
  FromJust ( 'Just x) = x

type family GetConstructors (t :: Type) :: NP I (ConstructorsOf t (GCode t)) where
  GetConstructors t = FromJust (UnsafeDeclConstructors t)

type SingliseCon :: forall (ct :: Type) (fieldst :: [Type]). ct -> NP I fieldst -> Type
type family SingliseCon (c :: ct) (fields :: NP I fieldst) :: Type where
  forall (a :: Type) (ct :: Type) (as :: [Type]) (x :: a) (xs :: NP I as) (c :: a -> ct).
    SingliseCon @(a -> ct) c ( 'I x ':* xs) =
      Single x -> SingliseCon (c x) xs
  SingliseCon c _ = Single c

witness :: c => Proxy c -> ()
witness c = let _ = witness c in ()

scon :: forall (ct :: Type) (constructor :: ct). ValidateConstructor constructor => Proxy constructor -> GiveForall (ConstructorArgs' ct) (SingliseCon constructor)
scon = let _ = witness (Proxy @(ValidateConstructor constructor)) in undefined

type MatcherFor :: forall (ct :: Type) (a :: Type) (fieldst :: [Type]). ct -> (a -> Type) -> NP I fieldst -> Type
type family MatcherFor (c :: ct) (p :: a -> Type) (fields :: NP I fieldst) :: Type where
  MatcherFor @(_ -> _) c p ( 'I x ':* xs) = Single x -> MatcherFor (c x) p xs
  MatcherFor c p _ = p c

newtype Matcher (p :: a -> Type) (c :: ct) = Matcher (GiveForall (ConstructorArgs' ct) (MatcherFor c p))

type Matchers' :: forall cts a. NP I cts -> (a -> Type) -> [Type]
type family Matchers' (cs :: NP I cts) (p :: a -> Type) :: [Type] where
  Matchers' 'Nil _ = '[]
  Matchers' ( 'I c ':* cs) p = Matcher p c ': Matchers' cs p

type family Matchers (a :: Type) (p :: a -> Type) :: [Type] where
  Matchers a p = Matchers' (GetConstructors a) p

smatch ::
  forall (a :: Type) (p :: a -> Type) (x :: a).
  Single x ->
  NP I (Matchers a p) ->
  p x
smatch = undefined

-- example

data MaybeUnit
  = NoUnit
  | JustUnit ()
  deriving stock (Generic, Show)
type instance UnsafeDeclConstructors MaybeUnit = 'Just ( 'I 'NoUnit ':* 'I 'JustUnit ':* 'Nil)

data MyUnit = MyUnit
  deriving stock (Generic, Show)
type instance UnsafeDeclConstructors MyUnit = 'Just ( 'I 'MyUnit ':* 'Nil)

_x :: Single 'MyUnit
_x = scon (Proxy @( 'MyUnit))

{-
type family Fst (x :: NP I [Type, Type]) :: Type where
  Fst ('I a ':* 'I b ':* 'Nil) = a

type family Snd (x :: NP I [Type, Type]) :: Type where
  Snd ('I a ':* 'I b ':* 'Nil) = b

swap :: forall (ab :: NP I [Type, Type]). (Fst ab, Snd ab) -> (Snd ab, Fst ab)
swap = undefined

class HasSingle a where
  type AsSingle a :: a -> Type
  switness :: Single (x :: a) -> AsSingle a x
  sprove :: AsSingle a x -> Single x

newtype Forall (f :: a -> Type) = Forall (forall (x :: a). f x)

data SForall (f :: a -> Type) :: Forall f -> Type where
  SForall ::
    forall (a :: Type) (f :: a -> Type) (y :: forall (x :: a). f x).
    Single @(forall (x :: a). f x) y ->
    SForall f ('Forall y)

instance HasSingle (Forall f) where
  type AsSingle (Forall f) = SForall f
  sprove (SForall x) = Single $ Forall (unSingle x)
  switness :: forall (a :: Type) (f :: a -> Type) (y :: Forall f). Single y -> SForall f y
  switness (Single (Forall x)) = lessUnsafeCoerce $ single @(forall (z :: a). f z) x \x' -> SForall x'

data Some (f :: a -> Type) = forall (x :: a). Some (f x)

data SSome (f :: a -> Type) :: Some f -> Type where
  SSome ::
    forall (a :: Type) (f :: a -> Type) (x :: a) (y :: f x).
    Single @(f x) y ->
    SSome f ('Some y)

instance HasSingle (Some f) where
  type AsSingle (Some f) = SSome f
  sprove (SSome x) = Single $ Some $ unSingle x
  switness :: forall (a :: Type) (f :: a -> Type) (y :: Some f). Single y -> SSome f y
  switness (Single (Some x)) = lessUnsafeCoerce $ single x \x' -> SSome x'

newtype Fix (f :: Type -> Type) = Fix (f (Fix f))

data SFix (f :: Type -> Type) :: Fix f -> Type where
  SFix :: Single x -> SFix f ('Fix x)

instance HasSingle (Fix f) where
  type AsSingle (Fix f) = SFix f
  sprove (SFix x) = Single $ Fix $ unSingle x
  switness (Single (Fix x)) = lessUnsafeCoerce $ single x \x' -> SFix x'

data SUnit :: () -> Type where
  SUnit :: SUnit '()

instance HasSingle () where
  type AsSingle () = SUnit
  sprove SUnit = Single ()
  switness (Single ()) = lessUnsafeCoerce SUnit

data Pair a b = Pair a b

data SPair a b :: Pair a b -> Type where
  SPair :: Single (x :: a) -> Single (y :: b) -> SPair a b ('Pair x y)

instance HasSingle (Pair a b) where
  type AsSingle (Pair a b) = SPair a b
  sprove (SPair x y) = Single $ Pair (unSingle x) (unSingle y)
  switness (Single (Pair x y)) = lessUnsafeCoerce $ single x \x' -> single y \y' -> SPair x' y'

data SEither a b :: Either a b -> Type where
  SLeft :: Single (x :: a) -> SEither a b ('Left x)
  SRight :: Single (y :: b) -> SEither a b ('Right x)

instance HasSingle (Either a b) where
  type AsSingle (Either a b) = SEither a b
  sprove (SLeft x) = Single $ Left $ unSingle x
  sprove (SRight y) = Single $ Right $ unSingle y
  switness (Single (Left x)) = lessUnsafeCoerce $ single x \x' -> SLeft x'
  switness (Single (Right y)) = lessUnsafeCoerce $ single y \y' -> SRight y'

newtype MyNatF a = MyNatF (Either () a) deriving stock Generic
newtype MyNat = MyNat (Fix MyNatF) deriving stock Generic

pattern N = MyNat (Fix (MyNatF (Left ())))

pattern S :: Fix MyNatF -> MyNat
pattern S x <- MyNat (Fix (MyNatF (Right (coerce -> x)))) where
  S x = MyNat (Fix (MyNatF (Right (coerce x))))

data IntOrBool :: MyNat -> Type where
  MkInt :: Int -> IntOrBool ('MyNat ('Fix ('MyNatF ('Left '()))))
  MkBool :: Bool -> IntOrBool ('MyNat ('Fix ('MyNatF ('Right n))))

type family GInner (rep :: Type -> Type) :: Type where
  GInner (D1 ( 'MetaData _ _ _ 'True) (C1 _ (S1 _ (Rec0 (x :: Type))))) = x

{- | This class provides a work-around for partially applying
 type families of kind @a@, where @a@ is either 'Type' or
 @b -> c@ where @c@ satisfies the same constraint.

 Given a type family @F : A -> Type@, you can make the following
 @
   type F' :: A -> Type
   newtype F' (a :: A) = F' (NoReduce (F a)) deriving stock Generic
 @
 It is then true that @forall a. Reduce (F' a) ~ F a@.
-}
type family Inner (x :: Type) :: Type where
  Inner x = GInner (Rep x)

type family FromInner (x :: Inner a) :: a where
type family ToInner (x :: a) :: Inner a where

newtype ViaInner (f :: a -> Type) (x :: Inner a) = ViaInner (f (FromInner x))

viaInner :: ViaInner f (ToInner x) -> f x
viaInner = unsafeCoerce

smatch :: forall (a :: Type) (x :: a) (p :: a -> Type). Single x -> (forall (y :: Inner a). Single (y :: Inner a) -> ViaInner p y) -> p x
smatch = undefined

f :: Single (x :: MyNat) -> IntOrBool x
f x = smatch x \y -> case switness y of
  SFix z -> smatch z \w -> undefined
-}

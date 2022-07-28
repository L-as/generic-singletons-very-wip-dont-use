# Generic singletons

Ever wanted to do dependently typed programming in Haskell without tons of TH?
Here you go!

## How to use

Your data type must be a strict SOP.
If you want recursion, universal or existential quantification, or GADTs,
you must use one of: `Forall`, `Some`, `Fix`, `:~:`.

You must then define the type instance `UnsafeDeclConstructors`.
If you make a mistake, this can lead to unsound behaviour,
since you might accidentally coerce incompatible types.

```haskell
data A = A1 Int | A2 Int Int | A3 Int () Int ()
  deriving stock Generic
type instance UnsafeDeclConstructors A = 'Just ('I A1 ':* 'I A2 ':* 'I A3 ':* 'Nil)
```

Then you have this:
```haskell
scon (Proxy @'A1) :: forall (i :: Int). Single i -> Single ('A1 i)
smatch (x :: Single (x :: A)) ::
  (forall (i :: Int). Single i -> p ('A1 i)) ->
  (forall (i1 :: Int) (i2 :: Int). Single i1 -> Single i2 -> p ('A2 i1 i2)) ->
  (forall (i1 :: Int) (u1 :: ()) (i2 :: Int) (u2 :: ()). Single i1 -> Single u1 -> Single i2 -> Single u2 -> p ('A3 i1 u1 i2 u2)) ->
  p x

known :: Known x => Proxy x -> Single x
know :: Single x -> (Known x => b) -> b
single :: a -> (Single (x :: a) -> b) -> b

type MkSingle :: forall (x :: a) -> Single x
type UnSingle :: forall a (x :: a). Single x -> a
```

# Future work

## Access to constructors from `Generic` instance

Given a `Generic` instance, you should be able to access the constructors
for a data type rather than just having access to things that are isomorphic to the
constructors.

Can be done through a GHC plugin potentially.

## Automatic `Generic`

There doesn't seem to be a reason for why it can't be that `Generic` could
be derived on-the-fly when you want a constraint `Generic a` for some `a`.
It could be treated like `Coercible`, in that it's only accessible if you
have the constructors in scope.

You could feasibly make some `GenericLike` thing with a plugin, albeit
this should really be done in GHC.

## Polytype on RHS of type family instance

This is quite annoying, and this restriction means we are currently limited to
data types with a limited number of constructors and fields per constructor.
Adding support for this would allow both a more succinct implementation along
with support for arbitrary SOPs.

### Alternative: Pattern matching on the type-level along with inference for it

You can often reformulate problems of the above sort as type families that
take in a single `forall` variable with type `NP I t`, where the type family
can choose `t` somehow. The reason this isn't usable in practice, is that
GHC can't disambiguate the variable via knowing what type family applications to it
result in, see https://discourse.haskell.org/t/returning-forall-from-type-family/4836.

## `Generic` for *all* data types

This seems feasible to implement, and would mean we can potentially
have singletons for much more complex types, albeit
even now you can emulate all data types using `Forall`, `Some`, `Fix`, and `:~:` AFAICT.

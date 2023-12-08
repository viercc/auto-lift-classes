# Revision history for auto-lift-classes

## 1.1 -- 2023-12-08

* Rename `AutoLift` module as `AutoLift.Coercible` and
  `AutoLift` module is now alias of `AutoLift.Coercible`.
* Add `AutoLift.Functor` module, which is almost drop-in replacement of `AutoLift.Coercible`,
  which uses `Functor f` instead of `(forall x y. Coercibe x y => Coercible (f x) (f y))`.
  Same for `Bifunctor f`.

## 1.0.1 -- 2023-04-06

* Fix the build failure on GHC 9.6 (#2)

## 1 -- 2023-01-12

* First version.

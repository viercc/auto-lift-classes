{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Derive lifted version of 'Show' or 'Read' classes, like @'Show1' f@ or @'Read1' f@,
--   from derivable instance @forall a. Show a => Show (f a)@.
module AutoLift.Functor
  ( Reflected1 (..),
    Reflected2 (..),

    -- * Reexports
    Show1 (..),
    Read (..),
    Read1 (..),
    ReadPrec,
  )
where

import AutoLift.Machinery
import Data.Coerce
import Data.Functor.Classes
import Text.Read
import Data.Bifunctor ( Bifunctor )

-- | A newtype wrapper to derive @'Show1' f@ and @'Read1' f@ from the following,
--   often derivable instance.
--
--   > instance Functor f
--   > instance Show a => Show (f a)
--   > instance Read a => Read (f a)
--
--   Unlike 'AutoLift.Coercible.Reflected1' from "AutoLift.Coercible" module, this wrapper
--   requires 'Functor' instance too.
--
-- ==== Example
--
-- Suppose you define a new type constructor @Foo@, and
-- derived its @Show@ and @Functor@ instance.
--
-- >>> :set -XDeriveFunctor
-- >>> data Foo a = Foo [a] Int a deriving (Show, Functor)
--
-- The derived @Show (Foo a)@ instance is defined for all @a@ with @Show a@ instance.
--
-- > instance Show a => Show (Foo a)
--
-- @Reflected1@ allows you to derive @'Show1' Foo@ instance from the above instance.
--
-- >>> :set -XStandaloneDeriving -XDerivingVia
-- >>> deriving via (Reflected1 Foo) instance Show1 Foo
--
-- Let's try the derived @Show1@ instance, by showing @Foo Bool@, where
-- @True@ is shown as @yes@ and @False@ as @no@, instead of the normal @Show Bool@ instance.
--
-- >>> import Text.Show (showListWith)
-- >>> let yesno b = (++) (if b then "yes" else "no")
-- >>> liftShowsPrec (const yesno) (showListWith yesno) 0 (Foo [True, False] 5 False) ""
-- "Foo [yes,no] 5 no"
newtype Reflected1 f a = Reflected1 (f a)

wrapShowDict1 :: ShowDict (f a) -> ShowDict (Reflected1 f a)
wrapShowDict1 = coerce

wrapReadDict1 :: ReadDict (f a) -> ReadDict (Reflected1 f a)
wrapReadDict1 = coerce

deriving newtype instance Show (f a) => Show (Reflected1 f a)

instance
  ( forall a. Show a => Show (f a),
    Functor f
  ) =>
  Show1 (Reflected1 f)
  where
  liftShowsPrec showsPrecB showListB =
    let showFB = wrapShowDict1 $ autoShow1Functor @f (ShowDict showsPrecB showListB)
     in _showsPrec showFB
  liftShowList showsPrecB showListB =
    let showFB = wrapShowDict1 $ autoShow1Functor @f (ShowDict showsPrecB showListB)
     in _showList showFB

deriving newtype instance Read (f a) => Read (Reflected1 f a)

instance
  ( forall a. Read a => Read (f a),
    Functor f
  ) =>
  Read1 (Reflected1 f)
  where
  liftReadPrec readPrecB readListPrecB =
    let readFB = wrapReadDict1 $ autoRead1Functor @f (ReadDict readPrecB readListPrecB)
     in _readPrec readFB

  liftReadListPrec readPrecB readListPrecB =
    let readFB = wrapReadDict1 $ autoRead1Functor @f (ReadDict readPrecB readListPrecB)
     in _readListPrec readFB

-- | A newtype wrapper to derive @'Show2' f@ and @'Read2' f@ from the following,
--   often derivable instance.
--
--   > instance (Show a, Show b) => Show (f a b)
--   > instance (Read a, Read b) => Read (f a b)
--
--   Unlike 'AutoLift.Coercible.Reflected2' from "AutoLift.Coercible" module, this wrapper
--   requires 'Data.Bifunctor.Bifunctor' instance too.
--
--   > instance Bifunctor f
--  
-- ==== Example
--
-- Suppose you define a new type constructor @Bar@, and
-- derived its @Show@ instance.
--
-- >>> data Bar a b = Bar [(Int,a,b)] deriving Show
--
-- The derived @Show (Bar a b)@ instance is defined for all @a@ and @b@ with @Show@ instances.
--
-- > instance (Show a, Show b) => Show (Bar a b)
--
-- By providing @Bifunctor@ instance, @Reflected2@ allows you to derive @'Show2' Bar@ instance
-- from the above instance.
--
-- >>> import Data.Bifunctor
-- >>> :set -XStandaloneDeriving -XDeriveFunctor -XDerivingVia
-- >>> deriving instance Functor (Bar a)
-- >>> instance Bifunctor Bar where bimap f g (Bar content) = Bar [ (i, f a, g b) | (i,a,b) <- content ]
-- >>> deriving via (Reflected2 Bar a) instance (Show a) => Show1 (Bar a)
-- >>> deriving via (Reflected2 Bar) instance Show2 Bar
--
-- Let's try the derived @Show2@ instance by showing @Bar Bool Char@, where
-- @True@ is shown as @yes@ and @False@ as @no@, instead of the normal @Show Bool@ instance.
--
-- >>> import Text.Show (showListWith)
-- >>> let yesno b = (++) (if b then "yes" else "no")
-- >>> liftShowsPrec2 (const yesno) (showListWith yesno) showsPrec showList 0 (Bar [(1, True, 'A'), (2, False, 'B')]) ""
-- "Bar [(1,yes,'A'),(2,no,'B')]"
newtype Reflected2 f a b = Reflected2 (f a b)

wrapShowDict2 :: ShowDict (f a b) -> ShowDict (Reflected2 f a b)
wrapShowDict2 = coerce

wrapReadDict2 :: ReadDict (f a b) -> ReadDict (Reflected2 f a b)
wrapReadDict2 = coerce

deriving newtype instance Show (f a b) => Show (Reflected2 f a b)

instance (forall y. Show y => Show (f a y), Functor (f a)) => Show1 (Reflected2 f a) where
  liftShowsPrec showsPrecB showListB =
    let showFAB = wrapShowDict2 $ autoShow1Functor @(f a) (ShowDict showsPrecB showListB)
     in _showsPrec showFAB
  
  liftShowList showsPrecB showListB = 
    let showFAB = wrapShowDict2 $ autoShow1Functor @(f a) (ShowDict showsPrecB showListB)
     in _showList showFAB

instance
  ( forall a b. (Show a, Show b) => Show (f a b),
    Bifunctor f
  ) =>
  Show2 (Reflected2 f)
  where
  liftShowsPrec2 showsPrecC showListC showsPrecD showListD =
    let showFCD = wrapShowDict2 $ autoShow2Bifunctor @f (ShowDict showsPrecC showListC) (ShowDict showsPrecD showListD)
     in _showsPrec showFCD
  liftShowList2 showsPrecC showListC showsPrecD showListD =
    let showFCD = wrapShowDict2 $ autoShow2Bifunctor @f (ShowDict showsPrecC showListC) (ShowDict showsPrecD showListD)
     in _showList showFCD

deriving newtype instance Read (f a b) => Read (Reflected2 f a b)

instance (forall y. Read y => Read (f a y),
          Functor (f a)) => Read1 (Reflected2 f a) where
  liftReadPrec readPrecB readListB =
    let readFAB = wrapReadDict2 $ autoRead1Functor @(f a) (ReadDict readPrecB readListB)
     in _readPrec readFAB
  
  liftReadListPrec readPrecB readListB =
    let readFAB = wrapReadDict2 $ autoRead1Functor @(f a) (ReadDict readPrecB readListB)
     in _readListPrec readFAB

instance
  ( forall a b. (Read a, Read b) => Read (f a b),
    Bifunctor f
  ) =>
  Read2 (Reflected2 f)
  where
  liftReadPrec2 readPrecC readListPrecC readPrecD readListPrecD =
    let readFCD = wrapReadDict2 $ autoRead2Bifunctor @f (ReadDict readPrecC readListPrecC) (ReadDict readPrecD readListPrecD)
     in _readPrec readFCD

  liftReadListPrec2 readPrecC readListPrecC readPrecD readListPrecD =
    let readFCD = wrapReadDict2 $ autoRead2Bifunctor @f (ReadDict readPrecC readListPrecC) (ReadDict readPrecD readListPrecD)
     in _readListPrec readFCD

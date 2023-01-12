{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Derive lifted version of 'Show' or 'Read' classes, like @'Show1' f@ or @'Read1' f@,
--   from derivable instance @forall a. Show a => Show (f a)@.
module AutoLift
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

-- | A newtype wrapper to derive @'Show1' f@ and @'Read1' f@ from the following,
--   often derivable instance.
--
--   > instance Show a => Show (f a)
--   > instance Read a => Read (f a)
--
-- ==== Example
--
-- Suppose you define a new type constructor @Foo@, and
-- derived its @Show@ instance.
--
-- >>> data Foo a = Foo [a] Int a deriving Show
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

instance
  ( forall a. Show a => Show (f a),
    forall xx yy. Coercible xx yy => Coercible (f xx) (f yy)
  ) =>
  Show1 (Reflected1 f)
  where
  liftShowsPrec showsPrecB showListB =
    let showFB = wrapShowDict1 $ autoShow1 @f (ShowDict showsPrecB showListB)
     in _showsPrec showFB
  liftShowList showsPrecB showListB =
    let showFB = wrapShowDict1 $ autoShow1 @f (ShowDict showsPrecB showListB)
     in _showList showFB

instance
  ( forall a. Read a => Read (f a),
    forall xx yy. Coercible xx yy => Coercible (f xx) (f yy)
  ) =>
  Read1 (Reflected1 f)
  where
  liftReadPrec readPrecB readListPrecB =
    let readFB = wrapReadDict1 $ autoRead1 @f (ReadDict readPrecB readListPrecB)
     in _readPrec readFB

  liftReadListPrec readPrecB readListPrecB =
    let readFB = wrapReadDict1 $ autoRead1 @f (ReadDict readPrecB readListPrecB)
     in _readListPrec readFB

-- | A newtype wrapper to derive @'Show2' f@ and @'Read2' f@ from the following,
--   often derivable instance.
--
--   > instance (Show a, Show b) => Show (f a b)
--   > instance (Read a, Read b) => Read (f a b)
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
-- @Reflected2@ allows you to derive @'Show2' Bar@ instance from the above instance.
--
-- >>> :set -XStandaloneDeriving -XDerivingVia
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

instance
  ( forall a b. (Show a, Show b) => Show (f a b),
    forall x1 y1 x2 y2.
    (Coercible x1 y1, Coercible x2 y2) =>
    Coercible (f x1 x2) (f y1 y2)
  ) =>
  Show2 (Reflected2 f)
  where
  liftShowsPrec2 showsPrecC showListC showsPrecD showListD =
    let showFCD = wrapShowDict2 $ autoShow2 @f (ShowDict showsPrecC showListC) (ShowDict showsPrecD showListD)
     in _showsPrec showFCD
  liftShowList2 showsPrecC showListC showsPrecD showListD =
    let showFCD = wrapShowDict2 $ autoShow2 @f (ShowDict showsPrecC showListC) (ShowDict showsPrecD showListD)
     in _showList showFCD

instance
  ( forall a b. (Read a, Read b) => Read (f a b),
    forall x1 y1 x2 y2.
    (Coercible x1 y1, Coercible x2 y2) =>
    Coercible (f x1 x2) (f y1 y2)
  ) =>
  Read2 (Reflected2 f)
  where
  liftReadPrec2 readPrecC readListPrecC readPrecD readListPrecD =
    let readFCD = wrapReadDict2 $ autoRead2 @f (ReadDict readPrecC readListPrecC) (ReadDict readPrecD readListPrecD)
     in _readPrec readFCD

  liftReadListPrec2 readPrecC readListPrecC readPrecD readListPrecD =
    let readFCD = wrapReadDict2 $ autoRead2 @f (ReadDict readPrecC readListPrecC) (ReadDict readPrecD readListPrecD)
     in _readListPrec readFCD

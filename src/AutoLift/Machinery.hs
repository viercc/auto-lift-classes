{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Internal workings of "AutoLift". You usually don't need to import
--   this module.
module AutoLift.Machinery (
    AdHoc(..),
    ShowDict(..), showDict, autoShow1, autoShow2,
    ReadDict(..), readDict, autoRead1, autoRead2
) where

import Data.Reflection
import Data.Proxy
import Data.Coerce
import Text.Read

-- | Apply ad hoc instances on type @a@.
newtype AdHoc s a = AdHoc a

-- * Show

-- | Injected dictionary of Show
data ShowDict a = ShowDict
  { _showsPrec :: Int -> a -> ShowS
  , _showList :: [a] -> ShowS
  }

showDict :: forall a. Show a => ShowDict a
showDict = ShowDict { _showsPrec = showsPrec, _showList = showList }
{-# INLINE showDict #-}

instance (Reifies s (ShowDict a)) => Show (AdHoc s a) where
  showsPrec = coerce $ _showsPrec (reflect (Proxy @s))
  {-# INLINABLE showsPrec #-}

  showList = coerce $ _showList (reflect (Proxy @s))
  {-# INLINABLE showList #-}

{-

u/Iceland_jack taught me the technique to use QuantifiedConstraint on Coercible constraint. Thanks!

https://www.reddit.com/r/haskell_jp/comments/a75z0s/blog_reflection%E3%82%92%E4%BD%BF%E3%81%A3%E3%81%9F%E3%83%86%E3%82%AF%E3%83%8B%E3%83%83%E3%82%AF/ed3efcv/

-}

-- | Automatic Show1
autoShow1 :: forall f b.
     (forall a. Show a => Show (f a))
  => (forall x y. Coercible x y => Coercible (f x) (f y))
  => ShowDict b
  -> ShowDict (f b)
autoShow1 showB = reify showB body
  where
    body :: forall name. Reifies name (ShowDict b) => Proxy name -> ShowDict (f b)
    body _ = coerce $ showDict @(f (AdHoc name b))
{-# INLINABLE autoShow1 #-}

-- | Automatic Show2
autoShow2 :: forall f c d.
     (forall a b. (Show a, Show b) => Show (f a b))
  => (forall x1 x2 y1 y2.
         (Coercible x1 y1, Coercible x2 y2) => Coercible (f x1 x2) (f y1 y2)
       )
  => ShowDict c
  -> ShowDict d
  -> ShowDict (f c d)
autoShow2 showC showD =
  reify showC $ \proxyC ->
    reify showD $ \proxyD ->
      body proxyC proxyD
  where
    body :: forall name1 name2. (Reifies name1 (ShowDict c), Reifies name2 (ShowDict d))
         => Proxy name1 -> Proxy name2 -> ShowDict (f c d)
    body _ _ = coerce $ showDict @(f (AdHoc name1 c) (AdHoc name2 d))
{-# INLINABLE autoShow2 #-}

-- * Read

-- | Injected dictionary of 'Read'
data ReadDict a = ReadDict
  { _readPrec :: ReadPrec a
  , _readListPrec :: ReadPrec [a]
  }

readDict :: forall a. Read a => ReadDict a
readDict = ReadDict{ _readPrec = readPrec, _readListPrec = readListPrec }
{-# INLINE readDict #-}

instance (Reifies s (ReadDict a)) => Read (AdHoc s a) where
  readPrec = coerce $ _readPrec (reflect (Proxy @s))
  {-# INLINABLE readPrec #-}
  readListPrec = coerce $ _readListPrec (reflect (Proxy @s))
  {-# INLINABLE readListPrec #-}

-- | Automatic Read1
autoRead1 :: forall f b.
     (forall a. Read a => Read (f a))
  => (forall x y. Coercible x y => Coercible (f x) (f y))
  => ReadDict b
  -> ReadDict (f b)
autoRead1 readB =
  reify readB body
  where
    body :: forall name. (Reifies name (ReadDict b)) => Proxy name -> ReadDict (f b)
    body _ = coerce (readDict @(f (AdHoc name b)))
{-# INLINABLE autoRead1 #-}

autoRead2 :: forall f c d.
     (forall a b. (Read a, Read b) => Read (f a b))
  => (forall x1 x2 y1 y2.
         (Coercible x1 y1, Coercible x2 y2) => Coercible (f x1 x2) (f y1 y2)
       )
  => ReadDict c
  -> ReadDict d
  -> ReadDict (f c d)
autoRead2 readC readD =
  reify readC $ \proxyC ->
    reify readD $ \proxyD ->
      body proxyC proxyD
  where
    body :: forall name1 name2. (Reifies name1 (ReadDict c), Reifies name2 (ReadDict d))
         => Proxy name1 -> Proxy name2 -> ReadDict (f c d)
    body _ _ = coerce (readDict @(f (AdHoc name1 c) (AdHoc name2 d)))
{-# INLINABLE autoRead2 #-}

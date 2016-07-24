{-|
  Module      : PackStream
  Description : Implementation of Neo4j's PackStream for the bolt protocol.
  Copyright   : (c) 2016, Stephen O'Brien
  License     : BSD3
  Maintainer  : Stephen O'Brien <wayofthepie@gmail.com>
  Stability   : experimental
  Portability : non-portable
  A type-level based implementation of the data types in Neo4j's packstream.
  @__Warning__@: This is a work in progress and is currently __very__ experimental.
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module PackStream.Internal.Types {-(
  MkNull
  , MkTinyInt
  , MkInitBytes
  , mkNull
  , mkTinyInt
  )-} where

import Data.Kind
import Data.Proxy
import Data.Type.Bool
import Data.Type.Equality
import Data.Type.List
import Data.Serialize.Put
import Data.Word
import GHC.TypeLits hiding (type (*))
-- $setup
-- >>> :set -XDataKinds


--------------------------------------------------------------------------------
-- Data declarations for message types.

-- | Placeholder for @__'Null'__@ message types.
data Null

-- | Placeholder for @__'Float64'__@ message types.
data Float64

-- | Placeholder for @__'TinyInt'__@ message types.
data TinyInt

-- | Placeholder for @__'BoolTrue'__@ message types, these correspond to
-- true boolean values.
data BoolTrue

-- | Placeholder for @__'BoolFalse'__@ message types, these correspond to
-- false boolean values.
data BoolFalse

data MaybeNeg a b
--------------------------------------------------------------------------------

-- | Custom type errors when type level size validation, with
-- @__'SizeValidator'__@ fails.
type family InvalidSizeErr t (l :: Nat) (u :: Nat) where
  InvalidSizeErr Null l u = TypeError (NullSizeOutOfBoundsError l u)

type GenericSizePutOfBoundsError n l u = ShowType n
  :<>: Text " types must have a size between "
  :<>: ShowType l
  :<>: Text " and "
  :<>: ShowType u

type NullSizeOutOfBoundsError l u = GenericSizePutOfBoundsError Null l u

-- | Type level size validation:
--
--  * @__'t'__@ is the type of the message whose size we are validating
--
--  * @__'l'__@ is the lower bound of the allowed size of this message type
--
--  * @__'u'__@ is the upper bound of the allowed size of this given message type
--
--  * @__'g'__@ is the /given/ size of this message type.
--
-- If validation of the size against the bounds succeeds @__'g'__@ will be type
-- used as the size, which is of kind @__'Nat'__@.
--
-- If validation fails a @__'TypeError'__@ corresponding to the type of
-- @__'t'__@ will be thrown.
--
-- @
-- SizeValidator Null 0 0 0
-- @
type family SizeValidator t (l :: Nat) (u ::Nat) (g :: Nat) where
  SizeValidator Null 0 0 a = If (CmpNat a 0 == EQ) a (InvalidSizeErr Null 0 0)

-- | The initial bytes for message type @__'m'__@ with size @__'s'__@ as a tuple
-- of two integers.
type family InitBytes (m :: *) (s :: Nat) :: (Nat,Nat) where
  InitBytes Null s = '(0xc0, SizeValidator Null 0 0 s)

type family ConsMessageInit (m :: *) (ms :: Maybe Nat) :: Nat where
  ConsMessageInit Null Nothing = InitByte Null

type family InitByte (m :: *) :: Nat where
  InitByte Null = 0xC0

type MkInitBytes a s = BuildInitBytes a s (InitBytes a s)

data BuildInitBytes a s (k :: (Nat, Nat)) :: Type where
  BuildInitBytes :: a -> Nat -> BuildInitBytes a s (InitBytes a s)

type MkNull = MkInitBytes Null 0

build :: forall a. KnownNat a => Proxy a -> Integer
build proxy = fromIntegral $ natVal (Proxy @a)

-- Awesome!
test = build (Proxy @(Snd (InitBytes Null 0)))

mkNull :: Integer
mkNull = fromIntegral . natVal $ Proxy @(Snd (InitBytes Null 0))



--- Testing


type family Foo a = b | b -> a where
  Foo Int = Bool

data Bar :: * -> * where
  Bar ::  a -> Bar (Foo a)


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

-- | This module provides generators for JSON values
--
--   Since the Value data type is an ADT with different alternatives and a recursive data type
--   we need to control how many of each variant (Number, String, Array, etc...) we generate
--   and how deeply we recurse when generating arrays and objects which themselves contain Values.
--
--   A registry is used to store all the generators and the current generation configuration
--
--   In order to control the recursivity we tag the type of Values we generate:
--
--     - Tag "Simple" Value is for generating Null, Number, String, Boolean
--     - Tag "Recurse" Value is for generating Array and Object
--
--   And we put a 'Depth` parameter in the registry. That parameter is decremented every time we generate values
--   for an Array or an Object
module Data.Registry.Hedgehog.AesonGenerators where

import Data.Aeson
import Data.Registry
import Data.Scientific as Scientific hiding (normalize)
import Data.String (fromString)
import Data.Vector as Vector (fromList)
import Hedgehog as H
import Hedgehog.Gen as Gen hiding (either)
import Hedgehog.Range as Range
import Protolude

-- | Generator for a default JSON value
genValue :: Gen Value
genValue = genValueFor simpleGens

-- | Generator for a JSON value with an adjusted set of generators
--
--   For example:
--    - change the recursive depth of the generation: genValueWith (setDepth 5)
--    - change the number of elements in an array or an object: genValueWith (setFieldsNb 5)
--    - change the generator used to generate field names in an object: genValueWith (setFieldNames (elements ["a", "b", "c"]))
--    - use a custom text generator: genValueWith (setGen myTextGenerator)
--    - change the range used for generating numbers: genValueWith (setRange (linear @Int 0 20))
genValueWith :: (Registry _ _ -> Registry _ _) -> Gen Value
genValueWith f = genValueFor (f simpleGens)

-- | Specialized generator for a number value
genNumberValue :: Gen Value
genNumberValue = unTag <$> make @(Gen (Tag "Number" Value)) simpleGens

-- | Generate a JSON value with a given set of generators to be used when recursing
genValueFor :: Registry _ _ -> Gen Value
genValueFor gens =
  case make @Depth gens of
    -- if the depth is 0 generate a Value from the `Simple` Value generation
    0 -> make @(Gen Value) gens
    -- if the depth is > 0 generate possibly recursive values like arrays and objects
    _ -> make @(Gen Value) $ recursiveGens (decrementDepth gens)

-- | Set of generators for JSON values including recursive values like arrays and objects
--   In order to control the recursivity of the Value data type we produce several types
--   for JSON values using tags:
--     `Recurse Value` is a generated value to be used when generating an array or an object
--     `Simple Value` is a generated value that is either: a String, a Number, a Bool, a Null value
recursiveGens :: Registry _ _ -> Registry _ _
recursiveGens overrides =
  normalize $
    -- generator choosing between generated arrays, objects or simple values
    gen genRecursiveValue
      -- generator for objects
      <: gen genObject
      -- generator for arrays
      <: gen genArray
      -- generator for field names (up to 3 by default)
      <: fun (listOf @FieldName 1 3)
      -- generator for the elements of arrays or objects (up to 3 by default)
      <: fun (listOf @(Tag "Recurse" Value) 1 3)
      -- generator for a JSON value to be used in an object or an array
      <: fun (tag <$> genValueFor overrides :: Gen (Tag "Recurse" Value))
      -- simple, non-recursive, generators
      <: overrides

-- | Set of generators for non-recursive JSON values
--   Those value are tagged as `Simple` but we can also extract a `Gen Value` from this list
simpleGens :: Registry _ _
simpleGens =
  gen untagSimpleValue
    <: gen genSimpleValue
    <: gen genNumber
    <: gen genString
    <: gen FieldName
    <: gen genText
    <: gen genBool
    <: gen genNull
    <: gen (linear 0 5 :: Range Int)
    <: gen (linear (-1000) 1000 :: Range Integer)
    <: val (Depth 3)

-- * Individual generators

-- | Create a generator for a Value which can possibly be recursive if it is an array or an object
genRecursiveValue :: Tag "Array" Value -> Tag "Object" Value -> Tag "Simple" Value -> Gen Value
genRecursiveValue arrayValue objectValue simpleValue = Gen.element [unTag arrayValue, unTag objectValue, unTag simpleValue]

-- | Drop the tag on a Value
untagSimpleValue :: Tag "Simple" Value -> Value
untagSimpleValue = unTag

-- | Create a generator for a non-recursive Value (i.e. not an array or an object)
genSimpleValue :: Tag "Null" Value -> Tag "Bool" Value -> Tag "Number" Value -> Tag "String" Value -> Gen (Tag "Simple" Value)
genSimpleValue nullValue boolValue numberValue stringValue =
  tag <$> Gen.element [unTag nullValue, unTag boolValue, unTag numberValue, unTag stringValue]

-- | Generator for the Null value
genNull :: Gen (Tag "Null" Value)
genNull = pure (tag Null)

-- | Generator for a boolean value
genBool :: Gen (Tag "Bool" Value)
genBool = tag . Bool <$> Gen.bool

-- | Generator for some Text
genText :: Range Int -> Gen Text
genText range = Gen.text range Gen.alphaNum

-- | Generator for a string value
genString :: Text -> Tag "String" Value
genString = tag . String

-- | Generator for a number value
genNumber :: Range Integer -> Gen (Tag "Number" Value)
genNumber range = fmap tag $ Number <$> (scientific <$> Gen.integral range <*> pure 0)

-- | Generator for an array value
genArray :: [Tag "Recurse" Value] -> Tag "Array" Value
genArray = tag . Array . Vector.fromList . fmap unTag

-- | Generator for an object value
genObject :: [FieldName] -> [Tag "Recurse" Value] -> Tag "Object" Value
genObject fields values = tag . object $ zip (fromString . toS . unFieldName <$> fields) (unTag <$> values)

-- * Support functions

-- | Simplification for funTo @Gen when adding a new function to the registry
gen :: forall a b. (ApplyVariadic Gen a b, Typeable a, Typeable b) => a -> Typed b
gen = funTo @Gen

-- | set a specific generator on top of the list of generators
setGen :: (Typeable a) => Gen a -> Registry _ _ -> Registry _ _
setGen g r = fun g +: r

-- | set a specific range on top of the list of generators
setRange :: (Typeable a) => Range a -> Registry _ _ -> Registry _ _
setRange range r = fun range +: r

-- | Generate a list of min' to max' elements
listOf :: forall a. Int -> Int -> Gen a -> Gen [a]
listOf min' max' = Gen.list (linear min' max')

-- | Simplification for setting a new recursion depth on the registry
setDepth :: Depth -> Registry _ _ -> Registry _ _
setDepth d r = normalize $ val d +: r

-- | Decrement the depth of generation during recursion
decrementDepth :: Registry _ _ -> Registry _ _
decrementDepth = tweak (\(d :: Depth) -> d - 1)

-- | Set the number of fields in an object
setFieldsNb :: Int -> Registry _ _ -> Registry _ _
setFieldsNb n r = fun (listOf @FieldName 1 n) +: r

-- | Set a generator for field names
setFieldNames :: Gen FieldName -> Registry _ _ -> Registry _ _
setFieldNames n r = fun n +: r

-- | Depth of generated Values
newtype Depth = Depth {unDepth :: Int}
  deriving newtype (Eq, Show, Num)

-- | Newtype for the name of fields in an object
newtype FieldName = FieldName {unFieldName :: Text}
  deriving newtype (Eq, Show, Read)

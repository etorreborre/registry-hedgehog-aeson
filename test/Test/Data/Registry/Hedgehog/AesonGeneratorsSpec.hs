{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Test.Data.Registry.Hedgehog.AesonGeneratorsSpec where

import Protolude
import Test.Tasty.Hedgehogx
-- uncomment to display the generated values
-- import Data.Registry.Hedgehog.AesonGenerators

-- | Sanity check
test_json_simple_values = prop "generate simple json values" $ do
  -- uncomment to display the generated values
  -- values <- forAll (genValueWith (setDepth 0))
  -- collect values
  success

test_json_complex_values = prop "generate complex json values" $ do
  -- uncomment to display the generated values
  -- values <- forAll (genValueWith (setDepth 2))
  -- collect values
  success

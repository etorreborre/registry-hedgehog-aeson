# `registry-hedgehog-aeson`

##### *It's functions all the way down* <img src="doc/images/unboxed-bottomup.jpg" border="0"/>

#### Presentation

This library is an add-on to [`registry`](https://github.com/etorreborre/registry), providing Hedgehog generators for Aeson values.

In order to generate a `Value` simply call `genValue`:
```
test_generate_value = prop "generate a json value" $ do
  value <- forAll genValue
  collect value
  success
```

You can modify many aspects of the generation by using the combinators in `Data.Registry.Hedgehog.AesonGenerators`:

  - change the recursive depth of the generation: `genValueWith (setDepth 5)`
  - change the number of elements in an array or an object: `genValueWith (setFieldsNb 5)`
  - use a custom text generator: `genValueWith (setGen myTextGenerator)`
  - change the range used for generating numbers: `genValueWith (setRange (linear @Int 0 20))`

For example:
```
test_complex_values = prop "generate complex json values" $ do
  value <- forAll (genValueWith (setDepth 10))
  collect value
  success
```

#### Relationship with `registry-hedgehog`

This library has been designed to only depend on `registry`.
You can add a dependency on `registry-hedgehog` if you want to create your own registry for data generators and add use the `Gen Value` provided by this library.

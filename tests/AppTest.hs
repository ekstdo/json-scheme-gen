{-# LANGUAGE OverloadedStrings #-}
module Main where


import Data.Aeson.Decoding (decode)
import qualified Data.Aeson.Types as JSONTypes
import Data.List
import qualified Data.Set as Set
import qualified Data.Aeson.KeyMap as KeyMap

import Test.HUnit
import qualified System.Exit as Exit
import JsonScheme

subsetTests = TestList [ 
         TestCase $ assertEqual "simple Primitive - Primitive Scheme comparison GT"
            (subset SNull SNumber)
            GT
         , TestCase $ assertEqual "simple Primitive - Primitive Scheme comparison EQ"
            (subset SNull SNull)
            EQ
         , TestCase $ assertEqual "simple Primitive - OR Scheme comparison"
            (subset SNull (SOr (Set.fromList [SBool, SNull])))
            LT
         , TestCase $ assertEqual "simple OR - Or Scheme comparison LT"
            (subset (SOr (Set.fromList [SBool, SNull])) (SOr (Set.fromList [SBool, SNull, SNumber])))
            LT
         , TestCase $ assertEqual "simple OR - Or Scheme comparison EQ"
            (subset (SOr (Set.fromList [SBool, SNull, SNumber])) (SOr (Set.fromList [SBool, SNull, SNumber])))
            EQ
         , TestCase $ assertEqual "simple Or - Or Scheme comparison GT"
            (subset (SOr (Set.fromList [SBool, SNull, SNumber])) (SOr (Set.fromList [SBool, SNull])))
            GT
    ]

tests = TestList [TestCase $ assertEqual "simple comparison" 
            (fromJSON <$> (decode "{\"a\": 1, \"b\": [2, null, true], \"c\": [{\"d\": 1, \"e\": true}, {\"f\": false}, {\"f\": null}]}" :: Maybe JSONTypes.Value))
            -- {"a": number, "b": [null | boolean | number], "c": [{"d": number, "e": boolean} | {"f": null | boolean}]}
            (Just $ SObject (KeyMap.fromList [("a",SNumber),("b",SArray (SOr (Set.fromList [SNull,SBool,SNumber]))),("c",SArray (SOr (Set.fromList [SObject (KeyMap.fromList [("d",SNumber),("e",SBool)]),SObject (KeyMap.fromList [("f",SOr (Set.fromList [SNull,SBool]))])])))]))
        , TestCase $ assertEqual "combining objects test" 
            (fromJSON <$> (decode "{\"c\": [[{\"a\": false}, {\"a\": null}], [{\"a\": false}, {\"b\": false}]]}" :: Maybe JSONTypes.Value))
            -- {"c": [[{"a": null | boolean} | {"b": boolean}]]}
            (Just $ SObject (KeyMap.fromList [("c",SArray (SArray (SOr (Set.fromList [SObject (KeyMap.fromList [("a",SOr (Set.fromList [SNull,SBool]))]),SObject (KeyMap.fromList [("b",SBool)])]))))]))

        ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

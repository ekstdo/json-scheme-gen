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

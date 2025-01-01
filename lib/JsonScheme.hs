{-# LANGUAGE LambdaCase #-}
module JsonScheme where 

import Data.Aeson.Decoding (decode)
import qualified Data.Aeson.Types as JSONTypes
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.These as These
import Data.List
import qualified Data.Set as Set
import qualified Data.Aeson.KeyMap as KeyMap

data Scheme = SNull
            | SBool
            | SString
            | SNumber
            | SArray Scheme
            | SObject !(KeyMap Scheme)
            | SOr (Set.Set Scheme) deriving (Eq, Ord, Show)

sor :: Scheme -> Scheme -> Scheme
sor SNull SNull = SNull
sor SBool SBool = SBool
sor SString SString = SBool
sor SNumber SNumber = SBool
sor (SArray s1) (SArray s2) = SArray (sor s1 s2)
sor (SObject s1) (SObject s2) = maybe
    (SOr $ Set.fromList [SObject s1, SObject s2])
    SObject
    (sequenceA (KeyMap.alignWith (These.these (const Nothing) (const Nothing) (\a b -> Just (sor a b))) s1 s2)  ) 
sor (SOr s1) (SOr s2) = SOr (sorMatch  s1 s2)
sor (SOr s1) b = SOr (sorMatch (Set.singleton b) s1)
sor b (SOr s1) = SOr (sorMatch (Set.singleton b) s1)
sor a b = SOr $ Set.fromList [a, b]

sorMatch :: Set.Set Scheme -> Set.Set Scheme -> Set.Set Scheme
sorMatch = foldr (\x accum -> case x of 
    SObject x' -> let (matching, notMatching) = Set.partition (\case
                                                            SObject y' -> KeyMap.keys x' == KeyMap.keys y'
                                                            _ -> False
                                                            ) accum in 
                                                                Set.insert (foldr sor x matching) notMatching
    _ -> Set.insert x accum )









-- this is actually a partial order and NOT a total order
-- because only the subset and equal relation are required
-- only LT and EQ are actually defined

allCmp EQ LT = LT
allCmp GT _  = GT
allCmp a  b  = if a == b then a else allCmp b a

anyCmp EQ LT = EQ
anyCmp GT b  = b
anyCmp a  b  = if a == b then a else anyCmp b a

subset SNull SNull = EQ
subset SBool SBool = EQ
subset SString SString = EQ
subset SNumber SNumber = EQ
subset (SArray s1) (SArray s2) = compare s1 s2
subset (SObject s1) (SObject s2) = foldr allCmp EQ (KeyMap.alignWith (These.these (const GT) (const GT) subset) s1 s2)
subset (SOr s1) (SOr s2) = if result == EQ then if length s1 == length s2 then EQ else LT else result
    where result = foldr (\x accum -> -- for every element in s1
            allCmp accum (s2 `hasSupersetOf` x)
            ) EQ s1
subset a (SOr s2) = if result == EQ then LT else result -- cannot be equal, as a isn't SOr
    where result = s2 `hasSupersetOf` a
subset a b = GT

hasSupersetOf b a = foldr (\y accum' -> -- go through every element in b
                let result = subset a y in 
                    anyCmp result accum'
            ) GT b

combined :: Set.Set Scheme -> Set.Set Scheme -> Set.Set Scheme
combined a b = Set.union (Set.filter (\x -> b `hasSupersetOf` x /= LT) a ) (Set.filter (\x -> b `hasSupersetOf` x == GT) a )

fromJSON :: JSONTypes.Value -> Scheme
fromJSON JSONTypes.Null = SNull
fromJSON (JSONTypes.Bool _) = SBool
fromJSON (JSONTypes.Number _) = SNumber
fromJSON (JSONTypes.String _) = SString
fromJSON (JSONTypes.Array a) = SArray $ if null a then SOr Set.empty else foldr1 sor (fromJSON <$> a)
fromJSON (JSONTypes.Object a) = SObject (fromJSON <$> a)

-- instance Show Scheme where
inlinePrint SNull = "null"
inlinePrint SBool = "boolean"
inlinePrint SString = "string"
inlinePrint SNumber = "number"
inlinePrint (SArray a) = "[" ++ inlinePrint a ++ "]"
inlinePrint (SOr a) = intercalate " | " ((fmap inlinePrint . Set.toList) a)
inlinePrint (SObject o) = "{" ++  intercalate ", " ((\(key, val) -> show key ++ ": " ++ inlinePrint val) <$> KeyMap.toList o)  ++ "}"

prettyPrint :: Int -> Scheme -> [Char]
prettyPrint indentLevel (SArray a) = "[\n" ++ replicate (indentLevel + 1) '\t' ++ prettyPrint (indentLevel + 1) a ++ "\n" ++ replicate indentLevel '\t' ++ "]"
prettyPrint indentLevel (SOr a) = intercalate " | " ((fmap (prettyPrint (indentLevel + 1)) . Set.toList) a)
prettyPrint indentLevel (SObject o) = "{\n" ++ replicate (indentLevel + 1) '\t' ++  intercalate (",\n" ++ replicate (indentLevel + 1) '\t') ((\(key, val) -> show key ++ ": " ++ prettyPrint (indentLevel + 1) val) <$> KeyMap.toList o) ++ "\n" ++ replicate indentLevel '\t'  ++ "}"
prettyPrint indentLevel a = inlinePrint a

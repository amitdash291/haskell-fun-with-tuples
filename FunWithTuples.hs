module FunWithTuples where

data TupleList = TupleStr [(String,String)] | TupleInt [(Integer,Integer)] | TupleDouble deriving (Show, Eq)
data IntStr = Int Integer | Str String | NotSupported deriving (Show, Eq)

mapTuples :: Show a => (a -> a -> a) -> [(a, a)] -> [a]
mapTuples f ts = map (\t -> f (fst t) (snd t)) ts

reduceTupleList :: TupleList  -> IntStr
reduceTupleList ts = case ts of
 TupleStr s -> Str (foldr (++) "" (mapTuples (++) s))
 TupleInt i -> Int (foldr (+) 0 (mapTuples (-) i))
 _ -> NotSupported
 
 -- Example usage: tupleList = TupleInt [(2,1), (5,1)]; reduceTupleList tupleList (output: 5)

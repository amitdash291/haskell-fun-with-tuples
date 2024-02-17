module FunWithTuples where

data Tuples = IntTuples [(Int,Int)] | StrTuples [(String,String)] | DoubleTuples[(Double, Double)] deriving (Show, Eq)
data NumStr = Num Int | Str String | NotSupported deriving (Show, Eq)

mapTuples :: Show a => (a -> a -> a) -> [(a, a)] -> [a]
mapTuples f ts = map (\t -> f (fst t) (snd t)) ts

mapStrTuples :: [(String, String)] -> [String]
mapStrTuples ts = intersperse ", " (mapTuples (\a b -> a ++ " " ++ b) ts)

reduceTuples :: Tuples  -> NumStr
reduceTuples t = case t of
  IntTuples tn -> Num (foldr (+) 0 (mapTuples (+) tn))
  StrTuples ts -> Str (foldr (++) "" (mapStrTuples ts))
  _ -> NotSupported
 
 -- Example usage
 main = do
  let intTuples = IntTuples [(2,1), (5,1)]
  putStrLn $ show $ reduceTuples intTuples -- output: Num 9
  let strTuples = StrTuples [("John","Doe"), ("Jane","Doe")]
  putStrLn $ show $ reduceTuples strTuples -- output: Str "John Doe, Jane Doe"
  let doubleTuples = DoubleTuples [(1.5,1.8), (2.2,1.4)]
  putStrLn $ show $ reduceTuples doubleTuples -- output: NotSupported

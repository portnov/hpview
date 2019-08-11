{-# LANGUAGE OverloadedStrings #-}

module Operations where

import Data.Maybe
import Data.List
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M

import Types

global :: T.Text
global = "GLOBAL"

parseName :: T.Text -> NameInfo
parseName name =
  case T.split (== ':') name of
    [single] -> NameInfo global single global single
    [pkg , name] ->
        let (mod, local) = T.breakOnEnd "." name
        in  NameInfo pkg name mod local
    _ -> NameInfo global name global name

allKeys :: Heap -> [T.Text]
allKeys h =
  S.toList $ S.unions [S.fromList (M.keys $ sampleItems sample) | sample <- heapSamples h]

calcNameWeight :: T.Text -> Heap -> Int
calcNameWeight name h =
  sum $ catMaybes [M.lookup name (sampleItems sample) | sample <- heapSamples h]

allKeysSorted :: Heap -> [T.Text]
allKeysSorted h = sortOn (negate . weight) keys
  where
    keys = allKeys h
    m = M.fromList [(key, calcNameWeight key h) | key <- keys]
    weight key = fromMaybe 0 $ M.lookup key m

allSamplesSorted :: Heap -> [Sample [Item]]
allSamplesSorted h =
    [Sample time (map toItem $ sortOn (negate . weight) (M.assocs items)) | Sample time items <- heapSamples h]
  where
    keys = allKeys h
    m = M.fromList [(key, calcNameWeight key h) | key <- keys]
    weight (key, _) = fromMaybe 0 $ M.lookup key m
    toItem (key, value) = Item key value

allSamples :: Heap -> [Sample [Item]]
allSamples h =
    [Sample time (map toItem (M.assocs items)) | Sample time items <- heapSamples h]
  where
    toItem (key, value) = Item key value

type SamplesData = [(T.Text, [(Double, (Int, Int))])]

allSamplesData :: Heap -> SamplesData
allSamplesData h = fromMap $ toMap $ concatMap convert sortedSamples
  where
    convert :: Sample [Item] -> [(T.Text, (Double, (Int, Int)))]
    convert sample = [(key, (sampleTime sample, range)) | (key, range) <- stitch (sampleItems sample)]

    stitch :: [Item] -> [(T.Text, (Int, Int))]
    stitch items =
      let values = tail $ scanl (+) 0 $ map itemValue items
      in  zip (map itemName items) $ zip (0 : values) values

    toMap :: [(T.Text, (Double, (Int, Int)))] -> M.Map T.Text [(Double, (Int, Int))]
    toMap list = foldr (\(k,v) m -> M.insertWith (++) k [v] m) M.empty list

    fromMap :: M.Map T.Text [(Double, (Int, Int))] -> [(T.Text, [(Double, (Int, Int))])]
    fromMap m =
      [(key, m M.! key) | key <- take 10 $ sortOn (negate . weight) keys]

    sortedSamples =
      [Sample time (map toItem $ sortOn (weight . fst) (extend items)) | Sample time items <- heapSamples h]

    extend items =
      let otherKeys = filter (`M.notMember` items) keys
      in  M.assocs items ++ [(key, 0) | key <- otherKeys]

    keys = allKeys h
    weights = M.fromList [(key, calcNameWeight key h) | key <- keys]
    weight key = fromMaybe 0 $ M.lookup key weights
    toItem (key, value) = Item key value

searchKey :: SamplesData -> Double -> Int -> Maybe T.Text
searchKey [] x y = Nothing
searchKey (plot : plots) x y =
    case check plot x (fromIntegral y) of
      Nothing -> searchKey plots x y
      Just key -> Just key
  where
    check (key, samples) x y =
      if checkSamples 0 (0,0) samples x y
        then Just key
        else Nothing

    interpolate x0 x1 y0 y1 x = (x - x0) * (y1 - y0) / (x1 - x0) + y0

    checkSamples _ _ [] _ _ = False
    checkSamples prevX (prevY1, prevY2) ((xn, (y1, y2)) : samples) x y =
      if prevX <= x && x <= xn
        then
          let y1' = interpolate prevX xn (fromIntegral prevY1) (fromIntegral y1) x
              y2' = interpolate prevX xn (fromIntegral prevY2) (fromIntegral y2) x
          in y1' <= y && y <= y2'
        else checkSamples xn (y1, y2) samples x y


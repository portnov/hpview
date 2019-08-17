{-# LANGUAGE OverloadedStrings #-}

module Operations where

import Data.Maybe
import Data.List
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M
import Text.Regex.TDFA
import Text.Regex.TDFA.Text () -- instances only

import Types

global :: T.Text
global = "GLOBAL"

trace_name :: T.Text
trace_name = "(trace elements)"

parseName :: T.Text -> NameInfo
parseName name =
  case T.split (== ':') name of
    [single] -> NameInfo global single global single
    [pkg , name] ->
        let (mod, local) = T.breakOnEnd "." name
        in  NameInfo pkg name mod local
    _ -> NameInfo global name global name

resampleHeap :: Maybe Int -> Heap -> Heap
resampleHeap Nothing heap = heap
resampleHeap (Just nTarget) heap = heap {heapSamples = resample (heapSamples heap)}
  where
    resample samples =
      let n = length samples
      in  if n <= nTarget
            then samples
            else let d = n `div` nTarget
                 in  head samples : each d (tail samples)

resampleData :: Int -> SamplesData -> SamplesData
resampleData nTarget datas =
  let n = length datas
  in  if n <= nTarget
        then datas
        else let d = n `div` nTarget
             in  head datas : each d (tail datas)
    
each :: Int -> [a] -> [a]
each d list = go 0 d list
  where
    go i d [] = []
    go i d (x : xs)
      | i == d = x : go 0 d xs
      | otherwise = go (i+1) d xs

filterHeap :: Maybe (Double, Double) -> Int -> TraceStyle -> Int -> (T.Text -> Bool) -> Bool -> Heap -> Heap
filterHeap mbTime n traceStyle tracePercent good showTrace heap =
    heap {heapSamples = map filterSample $ filter checkTime (heapSamples heap)}
  where
    keys = M.keys $ heapWeights heap
    sortedKeys = sortOn (negate . weight) keys
    goodKeys = S.fromList $ take keysCount $ filter good sortedKeys
    filterSample sample = sample {sampleItems = filterItems (sampleItems sample) }

    checkTime sample =
      case mbTime of
        Nothing -> True
        Just (from, to) -> from <= sampleTime sample && sampleTime sample <= to

    weights = heapWeights heap
    sortedWeights = map weight sortedKeys
    weight key = fromMaybe 0 $ M.lookup key weights

    -- TraceTotal: Select keys, which in total give at least (100 - tracePercent) of weight
    totalWeight = sum $ M.elems weights
    totalLimit = (1 - fromIntegral tracePercent / 100) * fromIntegral totalWeight
    runningWeights = scanl (+) 0 sortedWeights

    -- TraceEach: Select keys, each of which give more than tracePercent of weight
    eachLimit = (fromIntegral tracePercent / 100) * fromIntegral totalWeight

    bigKeysCount =
      case traceStyle of
        TraceTotal -> length $ takeWhile (< totalLimit) (map fromIntegral runningWeights)
        TraceEach -> length $ takeWhile (>= eachLimit) (map fromIntegral sortedWeights)

    -- of that, select first N keys
    keysCount = min bigKeysCount n

    filterItems items
      | showTrace =
        let (goodItems, trace) = M.partitionWithKey isGoodItem items
        in  goodItems `M.union` sumTrace trace
      | otherwise = M.filterWithKey isGoodItem items

    sumTrace items = M.singleton trace_name $ sum $ M.elems items

    isGoodItem key _ = key `S.member` goodKeys

allKeys :: Heap -> [T.Text]
allKeys h =
  S.toList $ S.unions [S.fromList (M.keys $ sampleItems sample) | sample <- heapSamples h]

calcNameWeight :: T.Text -> Heap -> Int
calcNameWeight name h
  | name == trace_name = minBound
  | otherwise = sum $ catMaybes [M.lookup name (sampleItems sample) | sample <- heapSamples h]

allKeysSorted :: Heap -> [T.Text]
allKeysSorted h = sortOn (negate . weight) keys
  where
    keys = allKeys h
    weight key = fromMaybe 0 $ M.lookup key (heapWeights h)

allSamplesSorted :: Heap -> [Sample [Item]]
allSamplesSorted h =
    [Sample time (map toItem $ sortOn (negate . weight) (M.assocs items)) | Sample time items <- heapSamples h]
  where
    keys = allKeys h
    weight (key, _) = fromMaybe 0 $ M.lookup key (heapWeights h)
    toItem (key, value) = Item key value

allSamples :: Heap -> [Sample [Item]]
allSamples h =
    [Sample time (map toItem (M.assocs items)) | Sample time items <- heapSamples h]
  where
    toItem (key, value) = Item key value

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
      [(key, m M.! key) | key <- sortOn (negate . weight) keys]

    sortedSamples =
      [Sample time (map toItem $ sortOn (weight . fst) (extend items)) | Sample time items <- heapSamples h]

    extend items =
      let otherKeys = filter (`M.notMember` items) keys
      in  M.assocs items ++ [(key, 0) | key <- otherKeys]

    keys = allKeys h
    weights = heapWeights h
    weight key = fromMaybe 0 $ M.lookup key weights
    toItem (key, value) = Item key value

searchKey :: SamplesData -> Double -> Int -> Maybe (T.Text, Double, Int)
searchKey [] x y = Nothing
searchKey (plot : plots) x y =
    case check plot x (fromIntegral y) of
      Nothing -> searchKey plots x y
      Just result -> Just result
  where
    check (key, samples) x y =
      case checkSamples 0 (0,0) samples x y of
        Just dy -> Just (key, x, dy)
        _ -> Nothing

    interpolate x0 x1 y0 y1 x = (x - x0) * (y1 - y0) / (x1 - x0) + y0

    checkSamples _ _ [] _ _ = Nothing
    checkSamples prevX (prevY1, prevY2) ((xn, (y1, y2)) : samples) x y =
      if prevX <= x && x <= xn
        then
          let y1' = interpolate prevX xn (fromIntegral prevY1) (fromIntegral y1) x
              y2' = interpolate prevX xn (fromIntegral prevY2) (fromIntegral y2) x
          in if y1' <= y && y <= y2'
               then Just (y2 - y1)
               else Nothing
        else checkSamples xn (y1, y2) samples x y

checkItem :: SearchField -> SearchMethod -> T.Text -> T.Text -> Bool
checkItem field method needle name
  | T.null needle = True
  | otherwise = 
      let info = parseName name
          value = case field of
                        Name -> niFullName info
                        Module -> niModule info
                        Package -> niPackage info
      in  case method of
            Contains -> needle `T.isInfixOf` value
            Exact -> needle == value
            Regexp -> value =~ needle


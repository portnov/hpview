
module Algebra where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Maybe
import Statistics.LinearRegression

import Types

selectKeyData :: T.Text -> [Sample ItemsMap] -> [Sample Int]
selectKeyData key samples = [sample {sampleItems = select sample} | sample <- samples]
  where
    select sample = fromMaybe 0 $ M.lookup key (sampleItems sample)

prepareSystem :: [Sample Int] -> (V.Vector Double, V.Vector Double)
prepareSystem samples =
      (V.fromList $ map sampleTime samples,
       V.fromList $ map (fromIntegral . sampleItems) samples)

growCoefficient :: T.Text -> [Sample ItemsMap] -> Double
growCoefficient key samples =
  let (xs, ys) = prepareSystem $ selectKeyData key samples
      (_, beta) = linearRegression xs ys
  in  beta


{-# LANGUAGE RecordWildCards #-}

module HotCRP.Config where

import Control.Monad.State.Strict
import Data.Function
import System.Random

import Lib.HotCRP
import Lib.Util

import Config

maxPaperReviewerLoad :: Config -> Int
maxPaperReviewerLoad conf@HC{..} =
  max_paper_reviewerload & max 1

maxPaperAuthorLoad :: Config -> Int
maxPaperAuthorLoad conf@HC{..} =
  max_paper_authorload & max 1

maxAreaPaperLoad :: Config -> Int
maxAreaPaperLoad conf@HC{..} =
  max_area_paperload & max 1

numAreas :: Config -> [Int]
numAreas conf@HC{..} =
  [ (fromIntegral s * area_ratio) & ceiling & max 1
  | s <- conf & sizes ]

numNonChairPC :: Config -> [Int]
numNonChairPC conf@HC{..} =
  [ (fromIntegral s * pc_ratio) & ceiling & max 0
  | s <- conf & sizes ]

numMereAuthors :: Config -> [Int]
numMereAuthors conf@HC{..} =
  [ (fromIntegral s * author_ratio) & ceiling & max 1
  | s <- conf & sizes ]

biasPCToNewArea :: Config -> Double
biasPCToNewArea conf@HC{..} =
  bias_pc_to_new_area & max 0.0 & min 1.0

randomAreaWithBias :: RandomGen g => [Area] -> [Area] -> Config -> State g Area
randomAreaWithBias allAreas newAreas conf = do
  bias <- state $ randomR (0.0, 1.0)
  if bias <= (conf & biasPCToNewArea)
    then randomElem newAreas
    else randomElem allAreas

hcFamilyToPool :: Family HotCRP -> Pools HotCRP
hcFamilyToPool = toPoolsGen mergeHotCRP

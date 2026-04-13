{-# LANGUAGE RecordWildCards #-}
module ProjMan.Config where

import Data.Function

import Lib.ProjMan

import Config

numProjects :: Config -> [Int]
numProjects conf@PM{..} =
  [ (fromIntegral s * project_ratio) & ceiling & max 1
  | s <- conf & sizes ]

numNonmanagerUsers :: Config -> [Int]
numNonmanagerUsers conf@PM{..} =
  [ (fromIntegral s * user_ratio) & ceiling & max 1
  | s <- conf & sizes ]

maxUserProjectLoad :: Config -> Int
maxUserProjectLoad conf@PM{..} =
  max_user_projectload & max 1

-- misc
pmFamilyToPool :: Family ProjMan -> Pools ProjMan
pmFamilyToPool = toPoolsGen mergeProjMan

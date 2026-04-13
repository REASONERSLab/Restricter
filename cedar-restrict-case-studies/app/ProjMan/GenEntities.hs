{-# LANGUAGE BangPatterns, ParallelListComp, ScopedTypeVariables, RecordWildCards #-}
module ProjMan.GenEntities where

import Data.Function

import Control.Monad
import Control.Monad.State.Strict
import System.Random

import Lib.ProjMan
import Lib.Util

import Config
import ProjMan.Config
import Sampling

-- aliases for readability
type PreProject = Int
type PreUser    = Int

type ProjectLoad = [Project]

-- Fixed values: projects, nonmanager users
generatePreEntities :: [Int] -> Family [Int]
generatePreEntities sizes =
  tabulateFamily
    (\ prevFam thisFam -> [prevFam..thisFam-1])
    sizes

generatePreProjects :: Config -> Family [PreProject]
generatePreProjects conf =
  generatePreEntities (conf & numProjects)

generatePreNonmanagerUsers :: Config -> Family [PreUser]
generatePreNonmanagerUsers conf =
  generatePreEntities (conf & numNonmanagerUsers)

randomProjectsWithManagers :: RandomGen g => Config -> State g (Family [(Project,User)])
randomProjectsWithManagers conf = do
  let projsFam = generatePreProjects conf
  [ forM projDelta
      (\ p -> do
         r <- randomElem roles
         return $ mkProjectWithManager (show p) r)
    | projDelta <- projsFam
    ] & sequence

randomNonmanagerUsers ::
  RandomGen g => Config -> Pools [Project] -> Family [PreUser]
              -> State g (Family [User])
randomNonmanagerUsers conf projectsPool usersFam =
  [ forM usersDelta
      (\ user -> do
         pl <- state $ randomR (1, conf & maxUserProjectLoad)
         projs <- randomElemsNoRepeat pl projects
         r <- randomElem roles
         return $ mkUser ("NM" ++ show user) r projs)
  | usersDelta <- usersFam
  | projects <- projectsPool
  ] & sequence

randomProjMan :: RandomGen g => Config -> State g (Family ProjMan)
randomProjMan conf@PM{..} = do
  let preProjsFam = conf & generatePreProjects
      preNMUsersFam = conf & generatePreNonmanagerUsers
  (projsFam, projMansFam) <-
    randomProjectsWithManagers conf
    & fmap (unzip . map unzip)
  nmUsersFam <- randomNonmanagerUsers conf (projsFam & toPools) preNMUsersFam
  return $
    [ ProjMan
      { developers   = usersDelta & filterUsersByRole roleDeveloper
      , planners     = usersDelta & filterUsersByRole rolePlanner
      , progmanagers = usersDelta & filterUsersByRole rolePM
      , accountants  = usersDelta & filterUsersByRole roleAccountant
      , projects     = projsDelta
      }
    | usersDelta <- zipWith (++) projMansFam nmUsersFam
    | projsDelta <- projsFam ]

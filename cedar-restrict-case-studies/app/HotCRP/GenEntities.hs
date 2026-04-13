{-# LANGUAGE ParallelListComp, RecordWildCards, ScopedTypeVariables #-}

module HotCRP.GenEntities where

import Control.Arrow
import Control.Monad
import Control.Monad.State.Strict
import Data.Function
import Debug.Trace
import System.Random

import Lib
import Lib.HotCRP
import Lib.Util

import Config
import HotCRP.Config

-- aliases for readability
type PreUser  = Int

-- fixed values: areas and non-chair PCs
generatePreEntities :: [Int] -> Family [Int]
generatePreEntities sizes =
  tabulateFamily
    (\ prevFam thisFam -> [prevFam..thisFam-1])
    sizes

generatePreNonChairPC :: Config -> Family [PreUser]
generatePreNonChairPC conf =
  generatePreEntities (conf & numNonChairPC)

generateAreas :: Config -> Family [Area]
generateAreas conf =
  generatePreEntities (conf & numAreas)
  & map (map (show >>> ('A':) >>> mkArea))

generateMereAuthors :: Config -> Family [User]
generateMereAuthors conf =
  generatePreEntities (conf & numMereAuthors)
  & map (map (\ i -> mkUser ("Auth" ++ show i) False False Nothing))

generateAreaChairs :: Family [Area] -> Family [User]
generateAreaChairs areasFam =
  [ areas
   & map (\ a -> mkUser ("Ch" ++ (a & uid & __entity & _id)) False True (Just a))
  | areas <- areasFam ]

-- generated very differently, only included in the first delta (and thus only
-- pulled from the first set of areas)
generatePCChair :: RandomGen g => Family [Area] -> State g (Family [User])
generatePCChair areasFam = sequence
  [ if i == 0 then do
      area <- randomElem areas
      return $ [mkUser "ChPC" True False (Just area)]
    else
      return []
  | areas <- areasFam
  | i <- [0..(areasFam & length)-1]
  ]

generateNonChairPC :: RandomGen g => Family [Area] -> Config -> State g (Family [User])
generateNonChairPC areasFam conf = sequence
  [ forM nonChairPC
      (\ pc -> do
          a <- conf & randomAreaWithBias areas newAreas
          return $ mkUser ("PC" ++ show pc) False False (Just a)
         )
  | areas <- areasFam & toPools
  | newAreas <- areasFam
  | nonChairPC <- conf & generatePreNonChairPC ]

randomPaperForArea :: RandomGen g => Config -> [User] -> [User] -> Int -> Area -> State g Paper
randomPaperForArea conf allUsers pc pid area = do
  let areaReviewers = area & potentialReviewersForArea pc
  reviewers <- do
    num <- state $ randomR (1, conf & maxPaperReviewerLoad)
    randomElemsNoRepeat num areaReviewers
  let potentialAuthors =
        allUsers & filter (\ u -> (u & uid) `notElem` (reviewers & map uid))
  authors <- do
    num <- state $ randomR (1, conf & maxPaperAuthorLoad)
    randomElemsNoRepeat num potentialAuthors
  return $ mkPaper pname authors reviewers area
  where
    pname :: String
    pname = (area & uid & __entity & _id) ++ "P" ++ show pid

generatePapers ::
  RandomGen g => Pools [User] -> Pools [User] -> Family [Area] -> Config -> State g (Family [Paper])
generatePapers allUsersPools pcPools areaFam conf = sequence
  [ forM areas
    (\ area -> do
        num <- state $ randomR (1, conf & maxAreaPaperLoad)
        forM [0..num-1]
          (\ i -> do
              area & randomPaperForArea conf allUsers pc i))
    & fmap concat
  | areas <- areaFam
  | allUsers <- allUsersPools
  | pc <- pcPools ]

generateReviewsForPaper :: RandomGen g => Config -> [User] -> [User] -> [Area] -> Paper -> State g [Review]
generateReviewsForPaper conf allUsers pc areas paper = do
  let reviewers = paper & reviewersForPaper pc
  metaReviewer <- randomElem reviewers
  let metaReview =
        mkReview
          (   "MR_" ++ (paper & uid & __entity & _id)
           ++ "_" ++ (metaReviewer & uid & __entity & _id))
          paper metaReviewer True
  let reviews =
        [ mkReview
            (   "R_" ++ (paper & getEntityName)
             ++ "_"  ++ (reviewer & getEntityName))
            paper reviewer False
        | reviewer <- reviewers ]
  return $ metaReview : reviews

generateReviews ::
  RandomGen g => Pools [User] -> Pools [User] -> Pools [Area]
              -> Family [Paper] -> Config -> State g (Family [Review])
generateReviews allUsersPools pcPools areaPools paperFam conf = sequence $
  [ forM newPapers
    (\ paper ->
        generateReviewsForPaper conf allUsers pc areas paper)
    & fmap concat
  | newPapers <- paperFam
  | allUsers <- allUsersPools
  | pc <- pcPools
  | areas <- areaPools ]

randomHotCRP :: RandomGen g => Config -> State g (Family HotCRP)
randomHotCRP conf = do
  let areasFam = conf & generateAreas
      mereAuthorsFam = conf & generateMereAuthors
      areaChairsFam  = areasFam & generateAreaChairs
  pcChairFam <- areasFam & generatePCChair
  nonChairPCFam <- conf & generateNonChairPC areasFam
  let pcFam =
        [ pcChair ++ areaChairs ++ nonChairPC
        | pcChair <- pcChairFam
        | areaChairs <- areaChairsFam
        | nonChairPC <- nonChairPCFam ]
  let allUsersFam =
        [ pc ++ mereAuthors
        | pc <- pcFam
        | mereAuthors <- mereAuthorsFam ]
  papersFam <-
    conf & generatePapers (allUsersFam & toPools) (pcFam & toPools) areasFam
  reviewsFam <-
    conf & generateReviews (allUsersFam & toPools) (pcFam & toPools) (areasFam & toPools) papersFam
  return
    [ HotCRP
      { mereAuthors = mereAuthors
      , programCommittee = pc
      , areas = areas
      , papers = papers
      , reviews = reviews
      }
    | mereAuthors <- mereAuthorsFam
    | pc <- pcFam
    | areas <- areasFam
    | papers <- papersFam
    | reviews <- reviewsFam
    ]

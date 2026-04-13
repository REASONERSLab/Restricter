{-# LANGUAGE RecordWildCards #-}

module Lib.HotCRP.Ops where

import Data.Function
import Data.List
import Data.Maybe

import Lib.Entity
import Lib.HotCRP.Types

-- non-HotCRP ops

potentialReviewersForArea :: [User] -> Area -> [User]
potentialReviewersForArea pc area =
  [ u | u <- pc, (u & attrs & pcMember & fromJust) == (area & uid) ]

areaForPaper :: [Area] -> Paper -> Area
areaForPaper areas paper =
  areas
  & find (\ a -> (a & uid) == (paper & attrs & area))
  & fromJust

reviewersForPaper :: [User] -> Paper -> [User]
reviewersForPaper us paper =
  [ u | u <- us, (u & uid) `elem` (paper & attrs & reviewers)]

-- HotCRP ops
getPCChair :: HotCRP -> User
getPCChair HotCRP{..} =
  programCommittee
  & find (\u -> u & attrs & isPCChair)
  & fromJust

getAreaChairs :: HotCRP -> [User]
getAreaChairs HotCRP{..} =
  programCommittee
  & filter (\ u -> u & attrs & isAreaChair)

getNonChairPC :: HotCRP -> [User]
getNonChairPC HotCRP{..} =
  programCommittee
  & filter
    (\ u -> not
      (  (u & attrs & isPCChair)
      || (u & attrs & isAreaChair) ))

getUsers :: HotCRP -> [User]
getUsers hc =
     (hc & programCommittee)
  ++ (hc & mereAuthors)

getPapersExcludeAuthors :: [User] -> HotCRP -> [Paper]
getPapersExcludeAuthors auths hc =
  hc & papers & filter
    (\ p ->
       p & attrs & authors & not . any
         (\ puid -> puid `elem` (auths & map uid)))

getPaperReviews :: Paper -> HotCRP -> [Review]
getPaperReviews paper hc =
  hc
  & reviews
  & filter (\ r -> (r & attrs & ofPaper) == (paper & uid))

getReviewerPapers :: User -> HotCRP -> [Paper]
getReviewerPapers u hc =
  hc
  & papers
  & filter (\ p -> (u & uid) `elem` (p & attrs & reviewers))

getReviewPaper :: Review -> HotCRP -> Paper
getReviewPaper r hc =
  hc
  & papers
  & find (\ p -> (p & uid) == (r & attrs & ofPaper))
  & fromJust

getAuthorPapers :: User -> HotCRP -> [Paper]
getAuthorPapers u hc =
  hc
  & papers
  & filter (\ p -> (u & uid) `elem` (p & attrs & authors))

-- partitionUsersByPCArea :: Area -> [User] -> ([User], [User])
-- partitionUsersByPCArea area =
--   partition $ \ user ->
--     case (user & attrs & pcMember) of
--       Nothing -> False
--       Just areaUID -> areaUID == (area & uid)

-- -- returns the designated reviewers of paper and PC chairs able to act on
-- -- reviews for paper
-- reviewersForPaper :: Paper -> [User] -> ([User], [User])
-- reviewersForPaper paper users = (reviewers', chairs)
--   where
--     reviewersPartition :: ([User], [User])
--     reviewersPartition =
--         users
--       & partition
--           (\ user ->
--              (user & uid) `elem` (paper & attrs & reviewers))

--     reviewers' = reviewersPartition & fst

--     chairs :: [User]
--     chairs =
--         reviewersPartition & snd
--       & filter
--           (\ user ->
--                 (user & attrs & isPCChair)
--              || (user & attrs & areaChair & maybe False (== (paper & attrs & area))))
--       & filter
--           (\ user ->
--              not $ (user & uid) `elem` (paper & attrs & reviewers))

-- getUser :: UID -> HotCRP -> User
-- getUser u HotCRP{..} =
--     users
--   & find (\user -> u == (user & uid))
--   & fromJust

-- getPaper :: UID -> HotCRP -> Paper
-- getPaper u HotCRP{..} =
--     papers
--   & find (\ paper -> u == (paper & uid))
--   & fromJust

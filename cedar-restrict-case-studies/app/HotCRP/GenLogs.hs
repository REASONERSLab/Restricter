{-# LANGUAGE ParallelListComp, RecordWildCards, TupleSections #-}
module HotCRP.GenLogs where

import Data.Function
import Data.Maybe
import Debug.Trace
import Control.Monad
import Control.Monad.State.Strict
import System.Random

import Lib
import Lib.HotCRP
import Lib.Util

import Config
import HotCRP.Config
import Sampling

-- Policy 1.
ipPCChairActOnPaper__Resources :: HotCRP -> [Paper]
ipPCChairActOnPaper__Resources hc =
  let pcChair = hc & getPCChair
  in hc & getPapersExcludeAuthors [pcChair]

ipPCChairActOnPaper :: RandomGen g => HotCRP -> State g HotCRPRequest
ipPCChairActOnPaper hc = do
  let pcChair = hc & getPCChair
  paper <- randomElem (hc & ipPCChairActOnPaper__Resources)
  act <- randomElem [Read, Update, Delete]
  return $ act & toAction & toRequest pcChair paper (HotCRPCtxt { isReleased = Nothing })

ipPCChairActOnReview__Resources :: HotCRP -> [Review]
ipPCChairActOnReview__Resources hc =
  let pcChair = hc & getPCChair
  in hc & getPapersExcludeAuthors [pcChair] & concatMap (`getPaperReviews` hc)

ipPCChairActOnReview :: RandomGen g => HotCRP -> State g HotCRPRequest
ipPCChairActOnReview hc = do
  let pcChair = hc & getPCChair
  review <- randomElem (hc & ipPCChairActOnReview__Resources)
  act <- randomElem [ReleaseReview, Read, Update, Delete]
  return $ act & toAction & toRequest pcChair review (HotCRPCtxt { isReleased = Nothing })

-- Area chairs acting on reviews
-- generalized
pAreaChairActOnReview ::
  RandomGen g => (HotCRP -> [(User,[Review])]) -> (HotCRP, HotCRP) -> Config -> State g HotCRPRequest
pAreaChairActOnReview sel (hcDelta, hc) conf = do
  areaChairsWithReviews <- conf & samplePrincipals sel (hcDelta, hc)
  (areaChair, reviews) <- randomElem areaChairsWithReviews
  review <- randomElem reviews
  act <- randomElem [Read, Update, Delete, ReleaseReview]
  return $ act & toAction & toRequest areaChair review (HotCRPCtxt Nothing)

-- - ideal privilege
ipAreaChairActOnReview__RSel :: User -> HotCRP -> [Review]
ipAreaChairActOnReview__RSel u hc =
  hc
  & getPapersExcludeAuthors [u]
  & filter (\ p -> (p & attrs & area) == (u & attrs & pcMember & fromJust))
  & concatMap (`getPaperReviews` hc)

ipAreaChairActOnReview__Sel :: HotCRP -> [(User,[Review])]
ipAreaChairActOnReview__Sel hc =
  hc
  & getAreaChairs
  & map (\ u -> (u, hc & ipAreaChairActOnReview__RSel u))
  & filter (\ (u,ps) -> not . null $ ps)

ipAreaChairActOnReview :: RandomGen g => (HotCRP, HotCRP) -> Config -> State g HotCRPRequest
ipAreaChairActOnReview = pAreaChairActOnReview ipAreaChairActOnReview__Sel

-- - over privilege
opAreaChairActOnReview__RSel :: User -> HotCRP -> [Review]
opAreaChairActOnReview__RSel u hc =
  hc
  & getPapersExcludeAuthors [u]
  & filter (\ p -> (p & attrs & area) /= (u & attrs & pcMember & fromJust))
  & concatMap (`getPaperReviews` hc)

opAreaChairActOnReview__Sel :: HotCRP -> [(User, [Review])]
opAreaChairActOnReview__Sel hc =
  hc
  & getAreaChairs
  & map (\ u -> (u, hc & opAreaChairActOnReview__RSel u))
  & filter (\ (_, rs) -> not . null $ rs)

opAreaChairActOnReview :: RandomGen g => (HotCRP, HotCRP) -> Config -> State g HotCRPRequest
opAreaChairActOnReview = pAreaChairActOnReview opAreaChairActOnReview__Sel

-- PC members reading reviews
pPCReadReview ::
  RandomGen g => (HotCRP -> [(User, [Review])]) -> (HotCRP, HotCRP) -> Config -> State g HotCRPRequest
pPCReadReview sel (hcDelta, hc) conf = do
  pcWithReviews <- conf & samplePrincipals sel (hcDelta, hc)
  (pcMem, reviews) <- randomElem pcWithReviews
  review <- randomElem reviews
  return $ Read & toAction & toRequest pcMem review (HotCRPCtxt Nothing)

-- - ideal privilege
ipPCReadReview__RSel :: User -> HotCRP -> [Review]
ipPCReadReview__RSel = ipAreaChairActOnReview__RSel

ipPCReadReview__Sel :: HotCRP -> [(User, [Review])]
ipPCReadReview__Sel hc =
  hc
  & getNonChairPC
  & map (\ u -> (u, hc & ipPCReadReview__RSel u))
  & filter (\ (_, rs) -> not . null $ rs)

ipPCReadReview ::
  RandomGen g => (HotCRP, HotCRP) -> Config -> State g HotCRPRequest
ipPCReadReview = pPCReadReview ipPCReadReview__Sel

-- - over privilege
opPCReadReview__RSel :: User -> HotCRP -> [Review]
opPCReadReview__RSel = opAreaChairActOnReview__RSel

opPCReadReview__Sel :: HotCRP -> [(User,[Review])]
opPCReadReview__Sel hc =
  hc
  & getNonChairPC
  & map (\ u -> (u, hc & opPCReadReview__RSel u))
  & filter (\ (_, rs) -> not . null $ rs)

opPCReadReview :: RandomGen g => (HotCRP, HotCRP) -> Config -> State g HotCRPRequest
opPCReadReview = pPCReadReview opPCReadReview__Sel

-- PC members reading papers
pPCReadPaper ::
  RandomGen g => (HotCRP -> [(User, [Paper])]) -> (HotCRP, HotCRP) -> Config -> State g HotCRPRequest
pPCReadPaper sel (hcDelta, hc) conf = do
  pcWithPapers <- conf & samplePrincipals sel (hcDelta, hc)
  (pcMem, papers) <- randomElem pcWithPapers
  paper <- randomElem papers
  return $ Read & toAction & toRequest pcMem paper (HotCRPCtxt Nothing)

-- - ideal privilege
ipPCReadPaper__RSel :: User -> HotCRP -> [Paper]
ipPCReadPaper__RSel u hc =
  hc
  & papers
  & filter (\ p -> (p & attrs & area) == (u & attrs & pcMember & fromJust))

ipPCReadPaper__Sel :: HotCRP -> [(User, [Paper])]
ipPCReadPaper__Sel hc =
  hc
  & getNonChairPC
  & map (\ u -> (u, hc & ipPCReadPaper__RSel u))
  & filter (\ (_, ps) -> not . null $ ps)

ipPCReadPaper :: RandomGen g => (HotCRP, HotCRP) -> Config -> State g HotCRPRequest
ipPCReadPaper = pPCReadPaper ipPCReadPaper__Sel

-- - over privilege

opPCReadPaper__RSel :: User -> HotCRP -> [Paper]
opPCReadPaper__RSel u hc =
   hc
  & papers
  & filter (\ p -> (p & attrs & area) /= (u & attrs & pcMember & fromJust))

opPCReadPaper__Sel :: HotCRP -> [(User, [Paper])]
opPCReadPaper__Sel hc =
  hc
  & getNonChairPC
  & map (\ u -> (u, hc & opPCReadPaper__RSel u))
  & filter (\ (_, ps) -> not . null $ ps)

opPCReadPaper :: RandomGen g => (HotCRP, HotCRP) -> Config -> State g HotCRPRequest
opPCReadPaper = pPCReadPaper opPCReadPaper__Sel

-- PC members updating reviews

pPCUpdateReview ::
  RandomGen g => (HotCRP -> [(User, [Review])]) -> (HotCRP, HotCRP) -> Config -> State g HotCRPRequest
pPCUpdateReview sel (hcDelta, hc) conf = do
  pcWithReviews <- conf & samplePrincipals sel (hcDelta, hc)
  (pcMem, reviews) <- randomElem pcWithReviews
  review <- randomElem reviews
  return $ Update & toAction & toRequest pcMem review (HotCRPCtxt Nothing)

-- - ideal privilege
ipPCUpdateReview__RSel :: User -> HotCRP -> [Review]
ipPCUpdateReview__RSel u hc =
  hc
  & reviews
  & filter (\ r -> (r & attrs & author) == (u & uid))

ipPCUpdateReview__Sel :: HotCRP -> [(User,[Review])]
ipPCUpdateReview__Sel hc =
  hc
  & getNonChairPC
  & map (\ u -> (u, hc & ipPCUpdateReview__RSel u))
  & filter (\ (_, rs) -> not . null $ rs)

ipPCUpdateReview :: RandomGen g => (HotCRP, HotCRP) -> Config -> State g HotCRPRequest
ipPCUpdateReview = pPCUpdateReview ipPCUpdateReview__Sel

-- over privilege
opPCUpdateReview__RSel :: User -> HotCRP -> [Review]
opPCUpdateReview__RSel u hc =
  hc
  & getReviewerPapers u
  & concatMap (`getPaperReviews` hc)
  & filter (\ r -> not $ (u & uid) == (r & attrs & author))

opPCUpdateReview__Sel :: HotCRP -> [(User,[Review])]
opPCUpdateReview__Sel hc =
  hc
  & getNonChairPC
  & map (\ u -> (u, hc & opPCUpdateReview__RSel u))
  & filter (\ (_, rs) -> not . null $ rs)

opPCUpdateReview :: RandomGen g => (HotCRP, HotCRP) -> Config -> State g HotCRPRequest
opPCUpdateReview = pPCUpdateReview opPCUpdateReview__Sel

-- Authors reading, updating, deleting their papers
pAuthActOnPaper ::
  RandomGen g => (HotCRP -> [(User, [Paper])]) -> (HotCRP, HotCRP) -> Config -> State g HotCRPRequest
pAuthActOnPaper sel (hcDelta, hc) conf = do
  authWithPapers <- conf & samplePrincipals sel (hcDelta, hc)
  (auth, papers)  <- randomElem authWithPapers
  paper <- randomElem papers
  act <- randomElem [Read, Update, Delete]
  return $ act & toAction & toRequest auth paper (HotCRPCtxt Nothing)

-- - ideal privilege
ipAuthActOnPaper__RSel :: User -> HotCRP -> [Paper]
ipAuthActOnPaper__RSel auth hc =
  hc
  & getAuthorPapers auth

ipAuthActOnPaper__Sel :: HotCRP -> [(User,[Paper])]
ipAuthActOnPaper__Sel hc =
  hc
  & getUsers
  & map (\ u -> (u, hc & ipAuthActOnPaper__RSel u))
  & filter (\ (_, ps) -> not . null $ ps)

ipAuthActOnPaper :: RandomGen g => (HotCRP, HotCRP) -> Config -> State g HotCRPRequest
ipAuthActOnPaper = pAuthActOnPaper ipAuthActOnPaper__Sel

-- - over privilege
-- - N/A

-- Authors reading reviews
pAuthReadReview ::
  RandomGen g => (HotCRP -> [(User,Bool,[Review])]) -> (HotCRP, HotCRP) -> Config -> State g HotCRPRequest
pAuthReadReview sel (hcDelta, hc) conf = do
  authWithReviews <- conf & samplePrincipals sel (hcDelta, hc)
  (auth, is, reviews) <- randomElem authWithReviews
  review <- randomElem reviews
  return $ Read & toAction & toRequest auth review (HotCRPCtxt (Just is))

-- - ideal privilege
ipAuthReadReview__RSel :: User -> HotCRP -> (Bool,[Review])
ipAuthReadReview__RSel u hc =
  hc
  & getAuthorPapers u
  & concatMap (`getPaperReviews` hc)
  & (True,)

ipAuthReadReview__Sel :: HotCRP -> [(User, Bool, [Review])]
ipAuthReadReview__Sel hc =
  hc
  & getUsers
  & map
    (\ u ->
        let (is, revs) = hc & ipAuthReadReview__RSel u
        in (u, is, revs))
  & filter (\ (_, _, rs) -> not . null $ rs)

ipAuthReadReview :: RandomGen g => (HotCRP, HotCRP) -> Config -> State g HotCRPRequest
ipAuthReadReview = pAuthReadReview ipAuthReadReview__Sel

-- - over privilege
opAuthReadReview__RSel :: User -> HotCRP -> (Bool, [Review])
opAuthReadReview__RSel u hc =
  hc
  & getAuthorPapers u
  & concatMap (`getPaperReviews` hc)
  & (False,)

opAuthReadReview__Sel :: HotCRP -> [(User, Bool, [Review])]
opAuthReadReview__Sel hc =
  hc
  & getUsers
  & map
    (\ u ->
        let (is, revs) = hc & opAuthReadReview__RSel u
        in (u, is, revs))
  & filter (\ (_, _, rs) -> not . null $ rs)

opAuthReadReview :: RandomGen g => (HotCRP, HotCRP) -> Config -> State g HotCRPRequest
opAuthReadReview = pAuthReadReview opAuthReadReview__Sel

createEventLog :: RandomGen g => Config -> Family HotCRP -> State g (Family [HotCRPRequest])
createEventLog conf hcFam = sequence
  [ do
      pcChairActPaper    <- randomPCChairActOnPaperReqs  hcDelta (hc, hcOld)
      pcChairActReview   <- randomPCChairActOnReviewReqs hcDelta (hc, hcOld)
      areaChairActReview <- randomAreaChairActOnReviewReqs hcDelta (hc, hcOld)
      pcReadReview       <- randomPCReadReviewReqs hcDelta (hc, hcOld)
      pcReadPaper        <- randomPCReadPaperReqs hcDelta (hc, hcOld)
      pcUpdateReview     <- randomPCUpdateReviewReqs hcDelta (hc, hcOld)
      authActOnPaper     <- randomAuthActOnPaperReqs hcDelta (hc, hcOld)
      authReadReview     <- randomAuthReadReviewReqs hcDelta (hc, hcOld)
      return $
           pcChairActPaper
        ++ pcChairActReview
        ++ areaChairActReview
        ++ pcReadReview
        ++ pcReadPaper
        ++ pcUpdateReview
        ++ authActOnPaper
        ++ authReadReview
  | hcDelta <- hcFam
  | hc      <- hcPool
  | hcOld   <- emptyHotCRP:(hcPool & init)
  ]
  where
    hcPool = hcFam & hcFamilyToPool

    cardPCChairActOnPaperActualPriv :: HotCRP -> (Int, Int)
    cardPCChairActOnPaperActualPriv hc =
      let ic = hc & ipPCChairActOnPaper__Resources & length
          oc = 0
      in (ic, oc)

    randomPCChairActOnPaperReqs :: RandomGen g => HotCRP -> (HotCRP, HotCRP) -> State g [HotCRPRequest]
    randomPCChairActOnPaperReqs hcDelta (hc, hcOld) = do
      let
        (idealNum, _) =
          ( conf & privilegeRepresentation (hc    & cardPCChairActOnPaperActualPriv)
          , conf & privilegeRepresentation (hcOld & cardPCChairActOnPaperActualPriv)
          ) & uncurry privilegeRepresentationDelta
      replicateM idealNum $ ipPCChairActOnPaper hc

    cardPCChairActOnReviewActualPriv :: HotCRP -> (Int, Int)
    cardPCChairActOnReviewActualPriv hc =
      let ic = hc & ipPCChairActOnReview__Resources & length
          oc = 0
      in (ic, oc)

    randomPCChairActOnReviewReqs :: RandomGen g => HotCRP -> (HotCRP, HotCRP) -> State g [HotCRPRequest]
    randomPCChairActOnReviewReqs hcDelta (hc, hcOld) = do
      let
        (idealNum, _) =
          ( conf & privilegeRepresentation (hc    & cardPCChairActOnReviewActualPriv)
          , conf & privilegeRepresentation (hcOld & cardPCChairActOnReviewActualPriv)
          ) & uncurry privilegeRepresentationDelta
      replicateM idealNum $ ipPCChairActOnReview hc

    cardAreaChairActOnReviewActualPriv :: HotCRP -> (Int, Int)
    cardAreaChairActOnReviewActualPriv hc =
      let ic =
            [ (a, r)
            | r <- hc & ipAreaChairActOnReview__Sel & map snd & concat
            , a <- [Read, Update, Delete, ReleaseReview]
            ] & length
          oc =
            [ (a, r)
            | r <- hc & opAreaChairActOnReview__Sel & map snd & concat
            , a <- [Read, Update, Delete, ReleaseReview]
            ] & length
      in (ic, oc)

    randomAreaChairActOnReviewReqs :: RandomGen g => HotCRP -> (HotCRP, HotCRP) -> State g [HotCRPRequest]
    randomAreaChairActOnReviewReqs hcDelta (hc, hcOld) = do
      let
        (idealNum, overNum) =
          ( conf & privilegeRepresentation (hc    & cardPCChairActOnReviewActualPriv)
          , conf & privilegeRepresentation (hcOld & cardPCChairActOnReviewActualPriv)
          ) & uncurry privilegeRepresentationDelta
      ideal <- replicateM idealNum $ ipAreaChairActOnReview (hcDelta, hc) conf
      over  <- replicateM overNum  $ opAreaChairActOnReview (hcDelta, hc) conf
      return $ ideal ++ over

    cardPCReadReviewActualPriv :: HotCRP -> (Int, Int)
    cardPCReadReviewActualPriv hc =
      let ic = [ r | r <- hc & ipPCReadReview__Sel & map snd & concat ] & length
          oc = [ r | r <- hc & opPCReadReview__Sel & map snd & concat ] & length
      in (ic, oc)

    randomPCReadReviewReqs :: RandomGen g => HotCRP -> (HotCRP, HotCRP) -> State g [HotCRPRequest]
    randomPCReadReviewReqs hcDelta (hc, hcOld) = do
      let
        (idealNum, overNum) =
          ( conf & privilegeRepresentation (hc    & cardPCReadReviewActualPriv)
          , conf & privilegeRepresentation (hcOld & cardPCReadReviewActualPriv)
          ) & uncurry privilegeRepresentationDelta
      ideal <- replicateM idealNum $ ipPCReadReview (hcDelta, hc) conf
      over  <- replicateM overNum  $ opPCReadReview (hcDelta, hc) conf
      return $ ideal ++ over

    cardPCReadPaperActualPriv :: HotCRP -> (Int, Int)
    cardPCReadPaperActualPriv hc =
      let ic = [ p | p <- hc & ipPCReadPaper__Sel & map snd & concat ] & length
          oc = [ p | p <- hc & opPCReadPaper__Sel & map snd & concat ] & length
      in (ic, oc)

    randomPCReadPaperReqs :: RandomGen g => HotCRP -> (HotCRP, HotCRP) -> State g [HotCRPRequest]
    randomPCReadPaperReqs hcDelta (hc, hcOld) = do
      let
        (idealNum, overNum) =
          ( conf & privilegeRepresentation (hc    & cardPCReadPaperActualPriv)
          , conf & privilegeRepresentation (hcOld & cardPCReadPaperActualPriv)
          ) & uncurry privilegeRepresentationDelta
      ideal <- replicateM idealNum $ ipPCReadPaper (hcDelta, hc) conf
      over  <- replicateM overNum  $ opPCReadPaper (hcDelta, hc) conf
      return $ ideal ++ over

    cardPCUpdateReviewActualPriv :: HotCRP -> (Int, Int)
    cardPCUpdateReviewActualPriv hc =
      let ic = [ r | r <- hc & ipPCUpdateReview__Sel & map snd & concat ] & length
          oc = [ r | r <- hc & opPCUpdateReview__Sel & map snd & concat ] & length
      in (ic, oc)

    randomPCUpdateReviewReqs :: RandomGen g => HotCRP -> (HotCRP, HotCRP) -> State g [HotCRPRequest]
    randomPCUpdateReviewReqs hcDelta (hc, hcOld) = do
      let
        (idealNum, overNum) =
          ( conf & privilegeRepresentation (hc    & cardPCUpdateReviewActualPriv)
          , conf & privilegeRepresentation (hcOld & cardPCUpdateReviewActualPriv)
          ) & uncurry privilegeRepresentationDelta
      ideal <- replicateM idealNum $ ipPCUpdateReview (hcDelta, hc) conf
      over  <- replicateM overNum  $ opPCUpdateReview (hcDelta, hc) conf
      return $ ideal ++ over

    cardAuthActOnPaperActualPriv :: HotCRP -> (Int, Int)
    cardAuthActOnPaperActualPriv hc =
      let ic = [ p | p <- hc & ipAuthActOnPaper__Sel & map snd & concat ] & length
          oc = 0
      in (ic, oc)

    randomAuthActOnPaperReqs :: RandomGen g => HotCRP -> (HotCRP, HotCRP) -> State g [HotCRPRequest]
    randomAuthActOnPaperReqs hcDelta (hc, hcOld) = do
      let
        (idealNum, _) =
          ( conf & privilegeRepresentation (hc    & cardAuthActOnPaperActualPriv)
          , conf & privilegeRepresentation (hcOld & cardAuthActOnPaperActualPriv)
          ) & uncurry privilegeRepresentationDelta
      replicateM idealNum $ ipAuthActOnPaper (hcDelta, hc) conf

    cardAuthReadReviewActualPriv :: HotCRP -> (Int, Int)
    cardAuthReadReviewActualPriv hc =
      let ic = [ r | r <- hc & ipAuthReadReview__Sel & map (\ (_, _, x) -> x) & concat ] & length
          oc = [ r | r <- hc & opAuthReadReview__Sel & map (\ (_, _, x) -> x) & concat ] & length
      in (ic, oc)

    randomAuthReadReviewReqs :: RandomGen g => HotCRP -> (HotCRP, HotCRP) -> State g [HotCRPRequest]
    randomAuthReadReviewReqs hcDelta (hc, hcOld) = do
      let
        (idealNum, overNum) =
          ( conf & privilegeRepresentation (hc    & cardAuthReadReviewActualPriv)
          , conf & privilegeRepresentation (hcOld & cardAuthReadReviewActualPriv)
          ) & uncurry privilegeRepresentationDelta
      ideal <- replicateM idealNum $ ipAuthReadReview (hcDelta, hc) conf
      over  <- replicateM overNum  $ opAuthReadReview (hcDelta, hc) conf
      return $ ideal ++ over

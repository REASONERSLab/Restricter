{-# LANGUAGE ParallelListComp, RecordWildCards, ScopedTypeVariables, TupleSections #-}
module ProjMan.GenLogs where

import Data.Function
import Data.List
import Control.Monad
import Control.Monad.State.Strict
import System.Random

import Lib
import Lib.ProjMan
import Lib.Util

import Config
import ProjMan.Config
import Sampling

-- It is possible (though usually unlikely) that the over privilege for a rule
-- is empty; if so, default to sampling from the ideal privilege using project
-- managers (these request generators are only called if there's some new delta
-- of projects, meaning there must be some project manager in the delta)
assemblePrincipals :: [ProjMan -> [User]] -> Bool -> ProjMan -> [User]
assemblePrincipals accessors projectManagersP pm =
  let managers = pm & projectManagers
      nonmanagers =
        accessors
        & concatMap ($ pm)
        & filter
            (\ u ->
               managers
               & not . any
                   (\ u' ->
                      (u' & uid) == (u & uid)))
  in
    if projectManagersP then
      managers ++ nonmanagers
    else
      nonmanagers

principalsFallback :: ProjMan -> [User] -> [User]
principalsFallback pm users | null users = pm & projectManagers
principalsFallback pm users | otherwise  = users

randomGenericRequest ::
  RandomGen g => (ProjMan -> [User]) -> (ProjMan, ProjMan) -> PMAction -> Config -> State g Request'
randomGenericRequest sel (pmDelta, pm) act conf = do
  principals <- conf & samplePrincipals sel (pmDelta, pm)
  user <- randomElem (principals & principalsFallback pm)
  project <- randomElem (user & getUserProjects pm)
  return $ act & toAction & toRequest' user project

ipViewBudgetPrincipals =
  assemblePrincipals [progmanagers, accountants, planners] True

ipViewBudget :: RandomGen g => ProjMan -> ProjMan -> Config -> State g Request'
ipViewBudget pmDelta pm =
  randomGenericRequest ipViewBudgetPrincipals (pmDelta, pm) ViewBudget

opViewBudgetPrincipals =
  assemblePrincipals [developers] False

opViewBudget :: RandomGen g => ProjMan -> ProjMan -> Config -> State g Request'
opViewBudget pmDelta pm =
  randomGenericRequest opViewBudgetPrincipals (pmDelta, pm) ViewBudget

ipEditBudgetPrincipals =
  assemblePrincipals [progmanagers,accountants] True

ipEditBudget :: RandomGen g => ProjMan -> ProjMan -> Config -> State g Request'
ipEditBudget pmDelta pm = do
  randomGenericRequest ipEditBudgetPrincipals (pmDelta, pm) EditBudget

opEditBudgetPrincipals =
  assemblePrincipals [developers,planners] False

opEditBudget :: RandomGen g => ProjMan -> ProjMan -> Config -> State g Request'
opEditBudget pmDelta pm = do
  randomGenericRequest opEditBudgetPrincipals (pmDelta, pm) EditBudget

-- no over privileges for ViewSchedule
ipViewSchedulePrincipals = users

ipViewSchedule :: RandomGen g => ProjMan -> ProjMan -> Config -> State g Request'
ipViewSchedule pmDelta pm =
  randomGenericRequest ipViewSchedulePrincipals (pmDelta, pm) ViewSchedule

ipEditSchedulePrincipals =
  assemblePrincipals [progmanagers] True

ipEditSchedule :: RandomGen g => ProjMan -> ProjMan -> Config -> State g Request'
ipEditSchedule pmDelta pm =
  randomGenericRequest ipEditSchedulePrincipals (pmDelta, pm) EditSchedule

opEditSchedulePrincipals =
  assemblePrincipals [planners, accountants, developers] False

opEditSchedule :: RandomGen g => ProjMan -> ProjMan -> Config -> State g Request'
opEditSchedule pmDelta pm =
  randomGenericRequest opEditSchedulePrincipals (pmDelta, pm) EditSchedule

ipViewAssetsPrincipals =
  assemblePrincipals [progmanagers, developers, planners] True

ipViewAssets :: RandomGen g => ProjMan -> ProjMan -> Config -> State g Request'
ipViewAssets pmDelta pm =
  randomGenericRequest ipViewAssetsPrincipals (pmDelta, pm) ViewAssets

opViewAssetsPrincipals =
  assemblePrincipals [accountants] False

opViewAssets :: RandomGen g => ProjMan -> ProjMan -> Config -> State g Request'
opViewAssets pmDelta pm =
  randomGenericRequest opViewAssetsPrincipals (pmDelta, pm) ViewAssets

ipEditAssetsPrincipals =
  assemblePrincipals [developers] True

ipEditAssets :: RandomGen g => ProjMan -> ProjMan -> Config -> State g Request'
ipEditAssets pmDelta pm =
  randomGenericRequest ipEditAssetsPrincipals (pmDelta, pm) EditAssets

opEditAssetsPrincipals =
  assemblePrincipals [progmanagers,accountants,planners] False

opEditAssets :: RandomGen g => ProjMan -> ProjMan -> Config -> State g Request'
opEditAssets pmDelta pm =
  randomGenericRequest opEditAssetsPrincipals (pmDelta, pm) EditAssets

-- no over privilege for view calendar
ipViewCalendarPrincipals = users

ipViewCalendar :: RandomGen g => ProjMan -> ProjMan -> Config -> State g Request'
ipViewCalendar pmDelta pm =
  randomGenericRequest ipViewCalendarPrincipals (pmDelta, pm) ViewCalendar

ipEditCalendarPrincipals =
  assemblePrincipals [planners] True

ipEditCalendar :: RandomGen g => ProjMan -> ProjMan -> Config -> State g Request'
ipEditCalendar pmDelta pm =
  randomGenericRequest ipEditCalendarPrincipals (pmDelta, pm) EditCalendar

opEditCalendarPrincipals =
  assemblePrincipals [developers,progmanagers,accountants] False

opEditCalendar :: RandomGen g => ProjMan -> ProjMan -> Config -> State g Request'
opEditCalendar pmDelta pm =
  randomGenericRequest opEditCalendarPrincipals (pmDelta, pm) EditCalendar

createEventLog :: RandomGen g => Config -> Family ProjMan -> State g (Family [Request'])
createEventLog conf pmFam =
  [ do
      budgetViews <- randomViewBudgetReqs pmDelta (pm,pmOld)
      budgetEdits <- randomEditBudgetReqs pmDelta (pm,pmOld)
      scheduleViews <- randomViewScheduleReqs pmDelta (pm,pmOld)
      scheduleEdits <- randomEditScheduleReqs pmDelta (pm,pmOld)
      assetViews <- randomViewAssetReqs pmDelta (pm,pmOld)
      assetEdits <- randomEditAssetReqs pmDelta (pm,pmOld)
      calendarViews <- randomViewCalendarReqs pmDelta (pm,pmOld)
      calendarEdits <- randomEditCalendarReqs pmDelta (pm,pmOld)
      return $
           budgetViews   ++ budgetEdits
        ++ scheduleViews ++ scheduleEdits
        ++ assetViews    ++ assetEdits
        ++ calendarViews ++ calendarEdits
  | pmDelta <- pmFam
  | pm <- pmPool
  | pmOld <- emptyProjMan:(pmPool & init)
  ] & sequence
  where
    pmPool = pmFam & pmFamilyToPool

    -- sample number calculation
    repActual :: Int -> Int
    repActual cardActual =
      (cardActual & fromIntegral) * (conf & priv_rep_ratio) & ceiling

    repOver :: Int -> Int
    repOver cardActual =
      (repActual cardActual) * (conf & over_priv_percent) `div` 100

    repIdeal :: Int -> Int
    repIdeal cardActual =
      (repActual cardActual) - (repOver cardActual)

    calcNumSamples :: (Int,Int) -> (Int,Int)
    calcNumSamples (actualPriv, oldActualPriv) =
      ( (actualPriv & repIdeal) - (oldActualPriv & repIdeal)
      , (actualPriv & repOver)  - (oldActualPriv & repOver))

    -- cardinality
    cardinality :: ProjMan -> ([User],[User]) -> Int
    cardinality pm (ipPrincipals,opPrincipals) =
      let ic =
            [ (user,project)
            | user <- ipPrincipals
            , project <- user & getUserProjects pm
            ] & length
          oc =
            [ (user, project)
            | user <- opPrincipals
            , project <- user & getUserProjects pm
            ] & length
      in
        ic + oc

    cardViewBudgetActualPriv :: ProjMan -> Int
    cardViewBudgetActualPriv pm =
      cardinality pm (ipViewBudgetPrincipals pm, opViewBudgetPrincipals pm)

    cardEditBudgetActualPriv :: ProjMan -> Int
    cardEditBudgetActualPriv pm =
      cardinality pm (ipEditBudgetPrincipals pm, opEditBudgetPrincipals pm)

    cardViewScheduleActualPriv :: ProjMan -> Int
    cardViewScheduleActualPriv pm =
      cardinality pm (ipViewSchedulePrincipals pm, [])

    cardEditScheduleActualPriv :: ProjMan -> Int
    cardEditScheduleActualPriv pm =
      cardinality pm (ipEditSchedulePrincipals pm, opEditSchedulePrincipals pm)

    cardViewAssetsActualPriv :: ProjMan -> Int
    cardViewAssetsActualPriv pm =
      cardinality pm (ipViewAssetsPrincipals pm, opViewAssetsPrincipals pm)

    cardEditAssetsActualPriv :: ProjMan -> Int
    cardEditAssetsActualPriv pm =
      cardinality pm (ipEditAssetsPrincipals pm, opEditAssetsPrincipals pm)

    cardViewCalendarActualPriv :: ProjMan -> Int
    cardViewCalendarActualPriv pm =
      cardinality pm (ipViewCalendarPrincipals pm, [])

    cardEditCalendarActualPriv :: ProjMan -> Int
    cardEditCalendarActualPriv pm =
      cardinality pm (ipEditCalendarPrincipals pm, opEditCalendarPrincipals pm)

    -- requests
    randomViewBudgetReqs ::
      RandomGen g => ProjMan -> (ProjMan,ProjMan) -> State g [Request']
    randomViewBudgetReqs pmDelta (pm, pmOld) = do
      let (idealNum, overNum) =
            (pm & cardViewBudgetActualPriv, pmOld & cardViewBudgetActualPriv)
            & calcNumSamples
      idealRep <- replicateM idealNum $ ipViewBudget pmDelta pm conf
      overRep  <- replicateM overNum  $ opViewBudget pmDelta pm conf
      return $ idealRep ++ overRep

    randomEditBudgetReqs ::
      RandomGen g => ProjMan -> (ProjMan, ProjMan) -> State g [Request']
    randomEditBudgetReqs pmDelta (pm, pmOld) = do
      let (idealNum, overNum) =
            (pm & cardEditBudgetActualPriv, pmOld & cardEditBudgetActualPriv)
            & calcNumSamples
      idealRep <- replicateM idealNum $ ipEditBudget pmDelta pm conf
      overRep  <- replicateM overNum  $ opEditBudget pmDelta pm conf
      return $ idealRep ++ overRep

    randomViewScheduleReqs ::
      RandomGen g => ProjMan -> (ProjMan, ProjMan) -> State g [Request']
    randomViewScheduleReqs pmDelta (pm, pmOld) = do
      let num =
            (pm & cardViewScheduleActualPriv, pmOld & cardViewScheduleActualPriv)
            & calcNumSamples
            & uncurry (+)
      replicateM num $ ipViewSchedule pmDelta pm conf

    randomEditScheduleReqs ::
      RandomGen g => ProjMan -> (ProjMan, ProjMan) -> State g [Request']
    randomEditScheduleReqs pmDelta (pm, pmOld) = do
      let (idealNum, overNum) =
            (pm & cardEditScheduleActualPriv, pmOld & cardEditScheduleActualPriv)
            & calcNumSamples
      idealRep <- replicateM idealNum $ ipEditSchedule pmDelta pm conf
      overRep  <- replicateM overNum  $ opEditSchedule pmDelta pm conf
      return $ idealRep ++ overRep

    randomViewAssetReqs ::
      RandomGen g => ProjMan -> (ProjMan, ProjMan) -> State g [Request']
    randomViewAssetReqs pmDelta (pm, pmOld) = do
      let (idealNum, overNum) =
            (pm & cardViewAssetsActualPriv, pmOld & cardViewAssetsActualPriv)
            & calcNumSamples
      idealRep <- replicateM idealNum $ ipViewAssets pmDelta pm conf
      overRep  <- replicateM overNum  $ opViewAssets pmDelta pm conf
      return $ idealRep ++ overRep

    randomEditAssetReqs ::
      RandomGen g => ProjMan -> (ProjMan, ProjMan) -> State g [Request']
    randomEditAssetReqs pmDelta (pm, pmOld) = do
      let (idealNum, overNum) =
            (pm & cardEditAssetsActualPriv, pmOld & cardEditAssetsActualPriv)
            & calcNumSamples
      idealRep <- replicateM idealNum $ ipEditAssets pmDelta pm conf
      overRep  <- replicateM overNum  $ opEditAssets pmDelta pm conf
      return $ idealRep ++ overRep

    randomViewCalendarReqs ::
      RandomGen g => ProjMan -> (ProjMan, ProjMan) -> State g [Request']
    randomViewCalendarReqs pmDelta (pm, pmOld) = do
      let num =
            (pm & cardViewCalendarActualPriv, pmOld & cardViewCalendarActualPriv)
            & calcNumSamples
            & uncurry (+)
      replicateM num $ ipViewCalendar pmDelta pm conf

    randomEditCalendarReqs ::
      RandomGen g => ProjMan -> (ProjMan, ProjMan) -> State g [Request']
    randomEditCalendarReqs pmDelta (pm, pmOld) = do
      let (idealNum, overNum) =
            (pm & cardEditCalendarActualPriv, pmOld & cardEditCalendarActualPriv)
            & calcNumSamples
      idealRep <- replicateM idealNum $ ipEditCalendar pmDelta pm conf
      overRep  <- replicateM overNum  $ opEditCalendar pmDelta pm conf
      return $ idealRep ++ overRep

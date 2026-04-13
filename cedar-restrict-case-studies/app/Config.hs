{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Config where

import Control.Monad.State.Strict
import Data.Function
import Data.List
import System.Console.CmdArgs

import Lib.HotCRP.Types

defaultDist :: FilePath
defaultDist = "./dist/"

defaultAssets :: FilePath
defaultAssets = "./assets/"

defaultSchema :: FilePath
defaultSchema = "schema.cedarschema"

defaultPolicies :: FilePath
defaultPolicies = "policies.cedar"

defaultLogBase :: FilePath
defaultLogBase = "logs"

defaultStoreBase :: String
defaultStoreBase = "entities"

type Family a = [a]
type Pools a   = [a]

tabulateFamily :: (Int -> Int -> a) -> [Int] -> Family a
tabulateFamily tab sizes =
  zipWith
    (\ prevFam thisFam -> tab prevFam thisFam)
    (0 : (sizes & init))
    sizes

mapFamily :: (a -> b) -> Family [a] -> Family [b]
mapFamily f = map (map f)

zipFamilyWith :: (a -> b -> c) -> Family [a] -> Family [b] -> Family [c]
zipFamilyWith f = zipWith (zipWith f)

toPools :: Family [a] -> Pools [a]
toPools = tail . scanl (++) []

toPoolsGen :: (a -> a -> a) -> Family a -> Pools a
toPoolsGen = scanl1

-- HotCRP contexts
hotCRPRequestCtxtFP :: Config -> HotCRPCtxt -> Maybe FilePath
hotCRPRequestCtxtFP conf (HotCRPCtxt Nothing) = Nothing
hotCRPRequestCtxtFP conf (HotCRPCtxt (Just False)) = Just $
  defaultAssets ++ "HotCRP/" ++ "context.unreleased.json"
hotCRPRequestCtxtFP conf (HotCRPCtxt (Just True)) = Just $
  defaultAssets ++ "HotCRP/" ++ "context.released.json"

data Config =
    GC
      { size_start :: Int
      , size_incr  :: Int
      , size_end   :: Int
      , priv_rep_ratio :: Double
      , over_priv_percent :: Int
      , sample_bias_new_principal :: Double

      , seed                  :: Int
      , policy_store          :: FilePath
      , entity_store_basename :: String
      , log_store_basename    :: String

      -- proportion to `size`
      , student_ratio :: Double
      , teacher_ratio :: Double
      , ta_ratio      :: Double

      -- constant factors
      , max_assignments_per_course :: Int
      , max_student_courseload     :: Int
      , max_teacher_courseload     :: Int
      , max_ta_courseload          :: Int
      }
  | PM
      { size_start :: Int
      , size_incr  :: Int
      , size_end   :: Int
      , priv_rep_ratio :: Double
      , over_priv_percent :: Int
      , sample_bias_new_principal :: Double

      , seed                  :: Int
      , policy_store          :: FilePath
      , entity_store_basename :: String
      , log_store_basename    :: String

      -- proportion to `size`
      , project_ratio :: Double
      , user_ratio    :: Double

      -- constant factors
      , max_user_projectload :: Int
      }
  | HC
    { size_start :: Int
    , size_incr  :: Int
    , size_end   :: Int
    , priv_rep_ratio :: Double
    , over_priv_percent :: Int
    , sample_bias_new_principal :: Double

    , seed                  :: Int
    , entity_store_basename :: String
    , policy_store          :: FilePath
    , log_store_basename    :: String

    -- proportion to `size`
    , area_ratio :: Double
    , pc_ratio    :: Double
    , author_ratio :: Double

    -- constant factors
    , max_paper_reviewerload :: Int
    , max_paper_authorload   :: Int
    , max_area_paperload     :: Int

    -- bias
    , bias_pc_to_new_area :: Double

    --   numPC         :: Int
    -- , numAreas      :: Int
    -- , numPCChairs   :: Int
    -- , numNonPC      :: Int
    -- , maxAreaPapers :: Int
    -- , maxPaperAuthors :: Int
    -- , maxPaperReviewers :: Int

    -- , seed          :: Int
    -- , entityStore   :: FilePath
    -- , policyStore   :: FilePath
    -- , logs          :: FilePath
    }
  deriving (Show, Data, Typeable)

sizes :: Config -> [Int]
sizes GC{..} = [size_start,(size_start+size_incr)..size_end]
sizes PM{..} = [size_start,(size_start+size_incr)..size_end]
sizes HC{..} = [size_start,(size_start+size_incr)..size_end]

entityStoreFP :: Config -> Family FilePath
entityStoreFP conf@GC{..} =
  [ defaultDist ++ "GClassroom/" ++ entity_store_basename ++ "." ++ show i ++ ".json"
  | i <- conf & sizes ]
entityStoreFP conf@PM{..} =
  [ defaultDist ++ "ProjMan/" ++ entity_store_basename ++ "." ++ show i ++ ".json"
  | i <- conf & sizes ]
entityStoreFP conf@HC{..} =
  [ defaultDist ++ "HotCRP/" ++ entity_store_basename ++ "." ++ show i ++ ".json"
  | i <- conf & sizes ]

logStoreFP :: Config -> Family FilePath
logStoreFP conf@GC{..} =
  [ defaultDist ++ "GClassroom/" ++ log_store_basename ++ "." ++ show i ++ ".json"
  | i <- conf & sizes ]
logStoreFP conf@PM{..} =
  [ defaultDist ++ "ProjMan/" ++ log_store_basename ++ "." ++ show i ++ ".json"
  | i <- conf & sizes ]
logStoreFP conf@HC{..} =
  [ defaultDist ++ "HotCRP/" ++ log_store_basename ++ "." ++ show i ++ ".json"
  | i <- conf & sizes ]

gclass = GC
  { size_start = 20
  , size_incr  = 10
  , size_end   = 40
  , priv_rep_ratio = 0.4  &= help "Ratio of privilege representation to actual privilege"
  , over_priv_percent = 5 &= help "Percentage of privilege representation that is over privilege"
  , sample_bias_new_principal = 0.5

  , student_ratio = 1.0 &= help "Student body to `size` ratio"
  , teacher_ratio = 0.1 &= help "Teacher to `size` ratio"
  , ta_ratio      = 0.1 &= help "TA to `size` ratio"

  , max_assignments_per_course = 3
  , max_student_courseload     = 5 &= help "Max courses in which a student enrolls"
  , max_teacher_courseload     = 2 &= help "Max courses run by a teacher"
  , max_ta_courseload          = 2 &= help "Max courses for a TA"

  , seed                  = 2025
  , entity_store_basename =    defaultStoreBase
                            &= typFile
  , policy_store          =    (defaultAssets ++ "GClassroom/" ++ defaultPolicies)
                            &= typFile
  , log_store_basename    =    defaultLogBase
  } &= help "Generate Cedar classroom case study"

projman = PM
  { size_start = 20
  , size_incr  = 10
  , size_end   = 40
  , priv_rep_ratio = 0.25 &= help "Ratio of privilege representation to actual privilege"
  , over_priv_percent = 5 &= help "Percentage of privilege representation that is over privilege"
  , sample_bias_new_principal = 0.5

  -- proportion to `size`
  , project_ratio = 0.1 &= help "Ratio of number of projects to `size`"
  , user_ratio    = 1.0 &= help "Ratio of number of users to `size`"

  -- constant factors
  , max_user_projectload = 3 &= help "Max projects for user"

  , seed                  = 2025
  , entity_store_basename =    defaultStoreBase
  , policy_store          =    (defaultAssets ++ "ProjMan/" ++ defaultPolicies)
                            &= typFile
  , log_store_basename    =    defaultLogBase
  } &= help "Generate Cedar project management case study"

hotcrp = HC
  { size_start = 20
  , size_incr  = 10
  , size_end   = 40
  , priv_rep_ratio = 0.25
  , over_priv_percent = 5
  , sample_bias_new_principal = 0.5

  , seed = 2025
  , entity_store_basename = defaultStoreBase
  , policy_store          = (defaultAssets ++ "HotCRP/" ++ defaultPolicies)
                            &= typFile
  , log_store_basename    = defaultLogBase

  , area_ratio   = 0.1
  , pc_ratio     = 0.2
  , author_ratio = 0.75

  , max_paper_reviewerload = 5
  , max_paper_authorload = 5
  , max_area_paperload = 10

  , bias_pc_to_new_area = 0.3
  } &= help "Generate Cedar HotCRP case study"

defaultConf =
     modes [gclass, projman, hotcrp]
  &= summary "Generate Cedar case studies"
  &= program "cedar-restrict-case-stuy"

preprocess :: Config -> Config
preprocess conf@GC{..} =
  conf
  { size_start = size_start'
  , size_incr  = size_incr'
  , size_end   = size_end'
  , priv_rep_ratio = priv_rep_ratio & max 0 & min 1
  , over_priv_percent = over_priv_percent & max 0 & min 100
  , sample_bias_new_principal = sample_bias_new_principal & max 0.0 & min 1.0
  }
  where
    size_start' = max size_start 0
    size_incr'  = max size_incr 1
    size_end'   = max size_end size_start'
preprocess conf@PM{..} =
  conf
  { size_start = size_start'
  , size_incr  = size_incr'
  , size_end   = size_end'
  , priv_rep_ratio = priv_rep_ratio & max 0 & min 1
  , over_priv_percent = over_priv_percent & max 0 & min 50
  }
  where
    size_start' = max size_start 0
    size_incr'  = max size_incr 1
    size_end'   = max size_end size_start'
preprocess conf@HC{..} =
  conf
  { size_start = size_start'
  , size_incr  = size_incr'
  , size_end   = size_end'
  , priv_rep_ratio = priv_rep_ratio & max 0 & min 1
  , over_priv_percent = over_priv_percent & max 0 & min 50
  }
  where
    size_start' = max size_start 0
    size_incr'  = max size_incr 1
    size_end'   = max size_end size_start'

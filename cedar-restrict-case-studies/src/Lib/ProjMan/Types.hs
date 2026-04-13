{-# LANGUAGE DeriveGeneric, RecordWildCards #-}

module Lib.ProjMan.Types where

import Data.Aeson
import Data.Function
import GHC.Generics

import Lib.Action
import Lib.Entity
import Lib.Request

type Role = Entity Value
mkRole :: String -> Role
mkRole name = Entity (mkUID "Role" name) (object []) []

roleDeveloper :: Role
roleDeveloper = mkRole "Developer"

rolePlanner :: Role
rolePlanner = mkRole "Planner"

rolePM :: Role
rolePM = mkRole "Program Manager"

roleAccountant :: Role
roleAccountant = mkRole "Accountant"

data UserAttrs = UserAttrs { role :: UID }
  deriving (Generic, Show, Eq)
instance ToJSON UserAttrs

type User = Entity UserAttrs

data ProjectAttrs = ProjectAttrs { manager :: UID }
  deriving (Generic, Show)
instance ToJSON ProjectAttrs

type Project = Entity ProjectAttrs
mkProject :: String -> User -> Project
mkProject name mngr =
  Entity
    (mkUID "Project" name)
    (ProjectAttrs (mngr & uid))
    []

mkUser :: String -> Role -> [Project] -> User
mkUser name rol projs =
  Entity
    (mkUID "User" name)
    (UserAttrs $ rol & uid)
    (map uid projs)

mkProjectWithManager :: String -> Role -> (Project,User)
mkProjectWithManager name rol =
  ( Entity projectUID (ProjectAttrs managerUID) []
  , Entity managerUID (UserAttrs $ rol & uid) [projectUID]
  )
  where
    projectUID :: UID
    projectUID = mkUID "Project" name

    managerUID :: UID
    managerUID = mkUID "User" ("PM" ++ name)

data ProjMan =
  ProjMan
  { developers   :: [User]
  , planners     :: [User]
  , progmanagers :: [User]
  , accountants  :: [User]
  , projects     :: [Project]
  }

emptyProjMan :: ProjMan
emptyProjMan =
  ProjMan
  { developers   = []
  , planners     = []
  , progmanagers = []
  , accountants  = []
  , projects     = []
  }

mergeProjMan :: ProjMan -> ProjMan -> ProjMan
mergeProjMan pm1 pm2 =
  ProjMan
  { developers = developers pm1 ++ developers pm2
  , planners = planners pm1 ++ planners pm2
  , progmanagers = progmanagers pm1 ++ progmanagers pm2
  , accountants = accountants pm1 ++ accountants pm2
  , projects = projects pm1 ++ projects pm2
  }

users :: ProjMan -> [User]
users ProjMan{..} = developers ++ planners ++ progmanagers ++ accountants

data PMAction =
    ViewBudget
  | EditBudget
  | ViewSchedule
  | EditSchedule
  | ViewAssets
  | EditAssets
  | ViewCalendar
  | EditCalendar
  deriving (Show, Eq, Ord)

toAction :: PMAction -> Action
toAction act = Action . show $ act

viewBudget :: User -> Project -> Request'
viewBudget u p = ViewBudget & toAction & toRequest' u p

editBudget :: User -> Project -> Request'
editBudget u p = EditBudget & toAction & toRequest' u p

data PMEntity =
    PMUser User
  | PMRole Role
  | PMProj Project
  deriving (Generic, Show)
instance ToJSON PMEntity where
  toJSON (PMUser u) = toJSON u
  toJSON (PMRole r) = toJSON r
  toJSON (PMProj p) = toJSON p

roles :: [Role]
roles = [roleDeveloper, rolePlanner, rolePM, roleAccountant]

toPMEntities :: ProjMan -> [PMEntity]
toPMEntities ProjMan{..} =
     map PMRole roles
  ++ map PMUser developers
  ++ map PMUser planners
  ++ map PMUser progmanagers
  ++ map PMUser accountants
  ++ map PMProj projects

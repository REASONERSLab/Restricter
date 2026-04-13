{-# LANGUAGE DeriveGeneric, RecordWildCards #-}

module Lib.ProjMan.Ops where

import Data.Function
import Data.List
import Data.Maybe

import Lib.Entity
import Lib.ProjMan.Types

-- non ProjMan operations
filterUsersByRole :: Role -> [User] -> [User]
filterUsersByRole r users =
  users & filter (\ u -> (u & attrs & role) == (r & uid))

filterUsersByProject :: Project -> [User] -> [User]
filterUsersByProject p us =
    us
  & filter (\u -> uid p `elem` parents u)

-- ProjMan operations
projectManagers :: ProjMan -> [User]
projectManagers pm =
  pm & users
  & filter
    (\ u ->
       pm & projects
       & any (\ p -> (p & attrs & manager) == (u & uid)))

getUserProjects :: ProjMan -> User -> [Project]
getUserProjects pm user =
  pm & projects & filter (\ p -> p `entityElem` (user & parents))

-- assignToProject :: ProjMan -> User -> Project -> ProjMan
-- assignToProject pm u p
--   | (u & attrs & role) == (roleDeveloper & uid) =
--     pm { developers =
--            developers pm
--          & (modifyAtUID (uid u) . addParent $ uid p)
--        }
--   | (u & attrs & role) == (rolePlanner & uid) =
--     pm { planners =
--            planners pm
--          & (modifyAtUID (uid u) . addParent $ uid p)
--        }
--   | (u & attrs & role) == (rolePM & uid) =
--     pm { progmanagers =
--            progmanagers pm
--          & (modifyAtUID (uid u) . addParent $ uid p)
--        }
--   | (u & attrs & role) == (roleAccountant & uid) =
--     pm { accountants =
--            accountants pm
--          & (modifyAtUID (uid u) . addParent $ uid p)
--        }
--   | otherwise = error $ "Unknown role: " ++ (show $ u & attrs & role)

-- assignToLeadProject :: ProjMan -> User -> Project -> ProjMan
-- assignToLeadProject pm u p =
--   pm { projects =
--          projects pm
--        & modifyAtUID (uid p)
--            (\ p -> p { attrs = ProjectAttrs (uid u) })
--      }

-- findProjectManager :: ProjMan -> Project -> User
-- findProjectManager pm p =
--     users pm
--   & find (\u -> uid u == (p & attrs & manager))
--   & fromJust

-- filterUsersByProject :: Project -> [User] -> [User]
-- filterUsersByProject p us =
--     us
--   & filter (\u -> uid p `elem` parents u)

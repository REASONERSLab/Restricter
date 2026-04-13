{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module Lib.GClassroom.Types where

import Data.Function
import Data.Aeson
import GHC.Generics

import Lib.Action
import Lib.Entity
import Lib.Request

type Course = Entity Value
mkCourse :: String -> Course
mkCourse name =
  Entity (mkUID "Course" name) (object []) []

-- getCourseEnrolled :: Course -> [UID]
-- getCourseEnrolled Entity{..} = enrolled attrs

type Student = Entity Value
mkStudent :: String -> [Course] -> Student
mkStudent name courses =
  Entity
    (mkUID "Student" name)
    (object [])
    (courses & map uid)

data AssignmentAttrs = AssignmentAttrs { course :: UID }
  deriving (Generic, Show)
instance ToJSON AssignmentAttrs

type Assignment = Entity AssignmentAttrs
mkAssignment :: String -> Course -> Assignment
mkAssignment name course =
  Entity (mkUID "Assignment" name) (AssignmentAttrs (uid course)) []

-- getAssignmentCourse :: Assignment -> UID
-- getAssignmentCourse Entity{..} = uid

data GradeAttrs =
  GradeAttrs { student :: UID, gassignment :: UID }
  deriving (Generic, Show)

instance ToJSON GradeAttrs where
  toJSON GradeAttrs{..} =
    object ["student" .= student, "assignment" .= gassignment ]

type Grade = Entity GradeAttrs
mkGrade :: String -> Student -> Assignment -> Grade
mkGrade name stud assign =
  Entity (mkUID "Grade" name)
    (GradeAttrs (stud & uid) (assign & uid)) []

type Role = Entity Value
mkRole :: String -> Role
mkRole name =  Entity (mkUID "Role" name) (object []) []

roleTeacher :: Role
roleTeacher = mkRole "Teacher"

roleTA :: Role
roleTA = mkRole "TA"

data StaffAttrs = StaffAttrs { role :: UID }
  deriving (Generic, Show)
instance ToJSON StaffAttrs

type Staff = Entity StaffAttrs
mkStaff :: String -> Role -> [Course] -> Staff
mkStaff name role courses =
  Entity (mkUID "Staff" name) (StaffAttrs (uid role)) (map uid courses)

mkTeacher :: String -> [Course] -> Staff
mkTeacher name courses = mkStaff name roleTeacher courses

mkTA :: String -> [Course] -> Staff
mkTA name courses = mkStaff name roleTA courses

data GClassroom = GClassroom
  { students    :: [Student]
  , courses     :: [Course]
  , assignments :: [Assignment]
  , grades      :: [Grade]
  , tas         :: [Staff]
  , teachers    :: [Staff]
  }
  deriving (Show)

emptyGClassroom :: GClassroom
emptyGClassroom =
  GClassroom
  { students    = []
  , courses     = []
  , assignments = []
  , grades      = []
  , tas         = []
  , teachers    = []
  }

mergeGClassroom :: GClassroom -> GClassroom -> GClassroom
mergeGClassroom gc1 gc2 =
  GClassroom
  { students    = (gc1 & students)    ++ (gc2 & students)
  , courses     = (gc1 & courses)     ++ (gc2 & courses)
  , assignments = (gc1 & assignments) ++ (gc2 & assignments)
  , grades      = (gc1 & grades)      ++ (gc2 & grades)
  , tas         = (gc1 & tas)         ++ (gc2 & tas)
  , teachers    = (gc1 & teachers)    ++ (gc2 & teachers)
  }

data GClassAction =
    PostAssignment
  | EditAssignment
  | PostGrade
  | ViewGrade
  deriving (Show)

toAction :: GClassAction -> Action
toAction = Action . show

postAssignment :: Staff -> Assignment -> Request'
postAssignment staf assign =
  PostAssignment & toAction & toRequest' staf assign

editAssignment :: Staff -> Assignment -> Request'
editAssignment staf assign =
  EditAssignment & toAction & toRequest' staf assign

postGrade :: Staff -> Grade -> Request'
postGrade staf gr =
  PostGrade & toAction & toRequest' staf gr

staffViewGrade :: Staff -> Grade -> Request'
staffViewGrade staf gr =
  ViewGrade & toAction & toRequest' staf gr

studentViewGrade :: Student -> Grade -> Request'
studentViewGrade stud gr =
  ViewGrade & toAction & toRequest' stud gr

data GClassEntity =
    GCCourse Course
  | GCRole Role
  | GCStaff Staff
  | GCStudent Student
  | GCAssignment Assignment
  | GCGrade Grade
  deriving (Generic, Show)

instance ToJSON GClassEntity where
  toJSON (GCCourse course) = toJSON course
  toJSON (GCRole role)     = toJSON role
  toJSON (GCStaff staff)   = toJSON staff
  toJSON (GCStudent stud)  = toJSON stud
  toJSON (GCAssignment assign) = toJSON assign
  toJSON (GCGrade grade)   = toJSON grade

toGClassEntities :: GClassroom -> [GClassEntity]
toGClassEntities GClassroom{..} =
     map GCRole [roleTeacher, roleTA]
  ++ map GCStudent students
  ++ map GCStaff (teachers ++ tas)
  ++ map GCCourse courses
  ++ map GCAssignment assignments
  ++ map GCGrade grades

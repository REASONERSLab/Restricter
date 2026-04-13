{-# LANGUAGE ParallelListComp, RecordWildCards, ScopedTypeVariables, TupleSections #-}

module GClassroom.GenEntities where

import Control.Monad
import Control.Monad.State.Strict
import Data.Function
import System.Random

import Lib
import Lib.GClassroom
import Lib.Util

import Config
import GClassroom.Config

-- Entity generation
--------------------------------------------------

-- aliases for readability
type PreStudent = Int
type PreTeacher = Int
type PreTA      = Int

type CourseCatalog = [Course] -- all courses
type CourseLoad    = [Course] -- courses of a student/teacher/ta

type CourseAssignments = [Assignment]
type CourseEnrollment  = [Student]

-- Fixed values: students, teachers, TAs
generatePreEntities :: [Int] -> Family [Int]
generatePreEntities sizes =
  tabulateFamily
    (\ prevFam thisFam -> [prevFam..thisFam-1])
    sizes

generatePreStudents :: Config -> Family [PreStudent]
generatePreStudents conf =
  generatePreEntities (conf & numStudents)

generatePreTeachers :: Config -> Family [PreTeacher]
generatePreTeachers conf =
  generatePreEntities (conf & numTeachers)

generatePreTAs :: Config -> Family [PreTA]
generatePreTAs conf =
  generatePreEntities (conf & numTAs)

-- random values: courses (and course loads)
--------------------------------------------------

-- Freshly generate random courses, grouped as course loads by the teacher
-- instructing them
randomCourseLoad :: RandomGen g => Config -> PreTeacher -> State g CourseLoad
randomCourseLoad conf teach = do
  numCourses <- state $ randomR (1, conf & maxTeacherCourseload)
  return $
    [ mkCourse ("T" ++ show teach ++ "C" ++ show (cno - 1))
    | cno <- [1..numCourses] ]

randomTeacherCourseLoads ::
  RandomGen g => Config -> Family [PreTeacher] -> State g (Family [CourseLoad])
randomTeacherCourseLoads conf teachFam = do
  forM teachFam
    (\ teachers ->
       forM teachers
         (\ teach ->
            randomCourseLoad conf teach))

-- Freshly generate a random course catalog, assigning courses to teachers to
-- create the teaching staff
randomCourseCatalogWithTeachers ::
  RandomGen g => Config -> Family [PreTeacher] -> State g (Family ([Staff], CourseCatalog))
randomCourseCatalogWithTeachers conf teachFam = do
  clsFam <- randomTeacherCourseLoads conf teachFam
  let catalogFam = clsFam & map concat
  let staffFam =
        zipFamilyWith
          (\ teach cl -> mkTeacher ("TE" ++ show teach) cl)
          teachFam clsFam
  return $ zip staffFam catalogFam

-- Randomly assign entities to courses after the courses have been generated
-- (students, TAs)
--
-- For family generation, the newer entities pull from the pool of older and
-- newer courses.
--
-- NOTE: it is possible that courses may not be assigned any such "ex post
-- facto" entities. In particular, this means there may be courses without
-- students.
randomPostFactoEntityCourseloads ::
  RandomGen g => Int -> Family CourseCatalog -> Family [Int]
                 -> State g (Family [CourseLoad])
randomPostFactoEntityCourseloads maxCourseLoad catalogFam entsFam = do
  let catalogPools = catalogFam & toPools
  zipWithM
    (\ ents catalog ->
       forM ents
         (\ _ -> do
            n <- state $ randomR (1, maxCourseLoad)
            randomElemsNoRepeat n catalog))
    entsFam catalogPools

randomPostFactoEntities ::
  RandomGen g => Int -> (Int -> CourseLoad -> a)
                 -> Family CourseCatalog -> Family [Int]
                 -> State g (Family [a])
randomPostFactoEntities maxCL mkEnt catalogFam entsFam = do
  courseloadFam <- randomPostFactoEntityCourseloads maxCL catalogFam entsFam
  return $ zipFamilyWith mkEnt entsFam courseloadFam

randomTAs ::
  RandomGen g => Config -> Family CourseCatalog -> Family [PreTA]
                 -> State g (Family [Staff])
randomTAs conf =
  randomPostFactoEntities (conf & maxTACourseload)
    (\ id cl -> mkTA ("TA" ++ show id) cl)

randomStudents ::
  RandomGen g => Config -> Family CourseCatalog -> Family [PreStudent]
                 -> State g (Family [Student])
randomStudents conf =
  randomPostFactoEntities (conf & maxStudentCourseload)
    (\ id cl -> mkStudent ("Stu" ++ show id) cl)

randomAssignmentsByCourse ::
  RandomGen g => Config -> Family CourseCatalog -> State g (Family [CourseAssignments])
randomAssignmentsByCourse conf catalogFam = do
  forM catalogFam
    (\ catalog ->
       forM catalog
         (\ course -> do
             numAssignments <- state $ randomR (1, conf & maxAssignmentsPerCourse)
             return $
               [ mkAssignment (assignId course i) course
               | i <-[0..numAssignments-1]]))
  where
    assignId :: Course -> Int -> String
    assignId c i = (c & uid & __entity & _id) ++ "A" ++ show i


generateGrades ::
  Config -> Pools CourseCatalog -> Pools [Assignment] -> Family [Student]
  -> Family [Grade]
generateGrades conf catalogPools assignmentPools studentsFam =
  [ gradeFamMember catalog assignments newStudents
  | catalog <- catalogPools
  | assignments <- assignmentPools
  | newStudents <- studentsFam
  ]
  where
    gradeId :: Student -> Assignment -> String
    gradeId s a =
      "G"
      ++ (a & uid & __entity & _id)
      ++ "S"
      ++ (s & uid & __entity & _id)

    gradeFamMember :: CourseCatalog -> [Assignment] -> [Student] -> [Grade]
    gradeFamMember catalog assignments newStudents = do
      stud <- newStudents
      assignment <- stud & findStudentAssignments catalog assignments
      return $ mkGrade (gradeId stud assignment) stud assignment

randomGClassroom :: RandomGen g => Config -> State g (Family GClassroom)
randomGClassroom conf = do
  let preStudentsFam = conf & generatePreStudents
      preTeachersFam = conf & generatePreTeachers
      preTAsFam      = conf & generatePreTAs
  (teachersFam, catalogFam) <-
    randomCourseCatalogWithTeachers conf preTeachersFam
    & (fmap unzip)
  tasFam <- randomTAs conf catalogFam preTAsFam
  studsFam <- randomStudents conf catalogFam preStudentsFam
  assignmentsFam :: Family [Assignment] <-
    randomAssignmentsByCourse conf catalogFam
    & fmap (map concat)
  let gradesFam =
        generateGrades conf
          (catalogFam & toPools) (assignmentsFam & toPools)
          studsFam
  return $
    [ GClassroom studs catalog assigns grds ts tchs
    | studs <- studsFam
    | catalog <- catalogFam
    | assigns <- assignmentsFam
    | grds <- gradesFam
    | ts <- tasFam
    | tchs <- teachersFam
    ]

{-# LANGUAGE FlexibleInstances, RecordWildCards #-}

module Lib.GClassroom.Ops where

import Data.Function
import Data.List
import Data.Maybe

import Lib.Entity
import Lib.GClassroom.Types

-- sloppily re-inventing lenses...

-- non-GC operations
findAssignmentCourse :: [Course] -> Assignment -> Course
findAssignmentCourse courses assignment =
  courses
  & find (\ c -> (c & uid) == (assignment & attrs & course))
  & fromJust

findCourseStudents :: [Student] -> Course -> [Student]
findCourseStudents students course =
  students
  & filter (\ s -> course `entityElem` (s & parents))

findCourseAssignments :: [Assignment] -> Course -> [Assignment]
findCourseAssignments assignments course' =
  assignments
  & filter (\ a -> (a & attrs & course) == (course' & uid))

findStaffCourses :: [Course] -> Staff -> [Course]
findStaffCourses courses staff =
  courses
  & filter (\ c -> c `entityElem` (staff & parents))

findStudentCourses :: [Course] -> Student -> [Course]
findStudentCourses courses student =
  courses
  & filter (\ c -> c `entityElem` (student & parents))

findStaffAssignments :: [Course] -> [Assignment] -> Staff -> [Assignment]
findStaffAssignments courses assignments staff = do
  c <- findStaffCourses courses staff
  findCourseAssignments assignments c

findStudentAssignments :: [Course] -> [Assignment] -> Student -> [Assignment]
findStudentAssignments courses assignments student = do
  c <- findStudentCourses courses student
  findCourseAssignments assignments c

findGradeAssignment :: [Assignment] -> Grade -> Assignment
findGradeAssignment assignments grade =
  assignments
  & find (\ a -> (a & uid) == (grade & attrs & gassignment))
  & fromJust

-- GC operations (for log generation)
class HasCourse a where
  getCourse :: GClassroom -> a -> Course

instance HasCourse Assignment where
  getCourse GClassroom{..} assignment =
    assignment & findAssignmentCourse courses

instance HasCourse Grade where
  getCourse gc@GClassroom{..} grade =
    grade & findGradeAssignment assignments & getCourse gc

class HasCourses a where
  getCourses :: GClassroom -> a -> [Course]

instance HasCourses Staff where
  getCourses GClassroom{..} staff = findStaffCourses courses staff

class HasStaff a where
  getStaffDistinguished :: GClassroom -> a -> (Staff,[Staff])

getStaff :: HasStaff a => GClassroom -> a -> [Staff]
getStaff gc x = x & getStaffDistinguished gc & uncurry (:)

getTeacher :: HasStaff a => GClassroom -> a -> Staff
getTeacher gc x = x & getStaffDistinguished gc & fst

instance HasStaff Course where
  getStaffDistinguished GClassroom{..} course =
    let s1 =
          teachers
          & find (\ t -> course `entityElem` (t & parents))
          & fromJust
        s2 =
          tas
          & filter (\ t -> course `entityElem` (t & parents))
    in
    (s1, s2)

instance HasStaff Assignment where
  getStaffDistinguished gc assignment =
    assignment & getCourse gc & getStaffDistinguished gc

class HasStudents a where
  getStudents :: GClassroom -> a -> [Student]

instance HasStudents Course where
  getStudents GClassroom{..} course =
      students
    & filter (\ s -> course `entityElem` (s & parents))

instance HasStudents Assignment where
  getStudents gc assignment =
    assignment
    & getCourse gc
    & getStudents gc

class HasAssignments a where
  getAssignments :: GClassroom -> a -> [Assignment]

instance HasAssignments Course where
  getAssignments GClassroom{..} c =
    c & findCourseAssignments assignments

instance HasAssignments Staff where
  getAssignments GClassroom{..} s =
    s & findStaffAssignments courses assignments

class HasGrades a where
  getGrades :: GClassroom -> a -> [Grade]

instance HasGrades Assignment where
  getGrades GClassroom{..} assignment =
    grades
    & filter (\ grade -> (grade & attrs & gassignment) == (assignment & uid))

instance HasGrades Course where
  getGrades gc@GClassroom{..} course =
    course
    & getAssignments gc
    & concatMap (getGrades gc)

instance HasGrades Staff where
  getGrades gc@GClassroom{..} s = do
    c <- s & getCourses gc
    c & getGrades gc

findGrade :: GClassroom -> Assignment -> Student -> Grade
findGrade gc@GClassroom{..} assign stud =
  assign
  & getGrades gc
  & find (\ gr -> (gr & attrs & student) == (stud & uid))
  & fromJust

getStudentGrades :: GClassroom -> Student -> [Grade]
getStudentGrades GClassroom{..} stud =
  grades
  & filter (\ gr -> (stud & uid) == (gr & attrs & student))

-- other
getAllStaff :: GClassroom -> [Staff]
getAllStaff GClassroom{..} = teachers ++ tas

getGradesNotForStaffMember :: GClassroom -> Staff -> [Grade]
getGradesNotForStaffMember gc@GClassroom{..} staffMem =
  grades
  & filter
      (\ gr ->
         let c = gr & getCourse gc
         in  not (c `entityElem` (staffMem & parents)))

{-# LANGUAGE RecordWildCards #-}
module GClassroom.Config where

import Data.Function

import Lib.GClassroom

import Config

numStudents :: Config -> [Int]
numStudents conf@GC{..} =
  [ (fromIntegral s * student_ratio) & ceiling & max 1
  | s <- conf & sizes ]

-- floor of teachers is 2, to guarantee that there exists a staff member and
-- course such that the staff member is not associated with that course
--
-- - a second teacher means there is at least one course not associated with a
--   given teacher (because all teachers have at least one course, and every
--   course has exactly one teacher)
numTeachers :: Config -> [Int]
numTeachers conf@GC{..} =
  [ (fromIntegral s * teacher_ratio) & floor & max 2
  | s <- conf & sizes ]

numTAs :: Config -> [Int]
numTAs conf@GC{..} =
  [ (fromIntegral s * ta_ratio) & floor & max 1
  | s <- conf & sizes ]

maxAssignmentsPerCourse :: Config -> Int
maxAssignmentsPerCourse GC{..} =
  max_assignments_per_course
  & max 1

maxStudentCourseload :: Config -> Int
maxStudentCourseload GC{..} =
  max_student_courseload
  & max 1

maxTeacherCourseload :: Config -> Int
maxTeacherCourseload GC{..} =
  max_teacher_courseload
  & max 1

maxTACourseload :: Config -> Int
maxTACourseload GC{..} =
  max_ta_courseload
  & max 1

-- misc
gcFamilyToPool :: Family GClassroom -> Pools GClassroom
gcFamilyToPool = toPoolsGen mergeGClassroom

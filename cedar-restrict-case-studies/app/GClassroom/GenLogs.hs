{-# LANGUAGE ParallelListComp, ScopedTypeVariables, RecordWildCards #-}
module GClassroom.GenLogs where

import Data.Function
import Control.Monad
import Control.Monad.State.Strict
import System.Random

import Lib
import Lib.GClassroom
import Lib.Util

import Config
import GClassroom.Config
import Sampling

-- requests
pPostAssignment :: RandomGen g => (GClassroom -> [Staff]) -> (GClassroom, GClassroom) -> Config -> State g Request'
pPostAssignment staffSel (gcDelta,gc) conf = do
  staff' <- conf & samplePrincipals staffSel (gcDelta, gc)
  staffMem <- randomElem staff'
  assignment <- randomElem (staffMem & getAssignments gc)
  return $ postAssignment staffMem assignment

pEditAssignment :: RandomGen g => (GClassroom -> [Staff]) -> (GClassroom, GClassroom) -> Config -> State g Request'
pEditAssignment staffSel (gcDelta,gc) conf = do
  staff' <- conf & samplePrincipals staffSel (gcDelta, gc)
  staffMem <- randomElem staff'
  assignment <- randomElem (staffMem & getAssignments gc)
  return $ editAssignment staffMem assignment

pPostGrade :: RandomGen g => (GClassroom -> [Staff]) -> (GClassroom, GClassroom) -> Config -> State g Request'
pPostGrade staffSel (gcDelta, gc) conf = do
  staff' <- conf & samplePrincipals staffSel (gcDelta, gc)
  staffMem <- randomElem staff'
  grade <- randomElem (staffMem & getGrades gc)
  return $ postGrade staffMem grade

ipViewGrade__Staff :: RandomGen g => (GClassroom -> [Staff]) -> (GClassroom, GClassroom) -> Config -> State g Request'
ipViewGrade__Staff staffSel (gcDelta, gc) conf = do
  staff' <- conf & samplePrincipals staffSel (gcDelta, gc)
  staffMem <- randomElem staff'
  grade <- randomElem (staffMem & getGrades gc)
  return $ staffViewGrade staffMem grade

-- NOTE: There is no guarantee that a particular staff member has grades not
-- associated with them. Entity generation only guarantees there is at least one
-- staff member (in the pool) with this property
opViewGrade__Staff :: RandomGen g => (GClassroom -> [(Staff,[Grade])]) -> (GClassroom, GClassroom) -> Config -> State g Request'
opViewGrade__Staff staffSel (gcDelta, gc) conf = do
  staff' <- conf & samplePrincipals staffSel (gcDelta, gc)
  (staffMem, gradesNotForThem) <- randomElem staff'
  grade <- randomElem gradesNotForThem
  return $ staffViewGrade staffMem grade


pViewGrade__Student :: RandomGen g => (GClassroom -> [Student]) -> (GClassroom, GClassroom) -> Config -> State g Request'
pViewGrade__Student studSel (gcDelta, gc) conf = do
  studs' <- conf & samplePrincipals studSel (gcDelta, gc)
  stud <- randomElem studs'
  grade <- randomElem (stud & getStudentGrades gc)
  return $ studentViewGrade stud grade

createEventLog ::
  RandomGen g => Config -> Family GClassroom -> State g (Family [Request'])
createEventLog conf gcFam =
  [ do
      postAssigns <- randomPostAssignmentReqs gcFamMem (gc, gcOld)
      editAssigns <- randomEditAssignmentReqs gcFamMem (gc, gcOld)
      postGrades  <- randomPostGradeReqs      gcFamMem (gc, gcOld)
      viewGradesStaff <- randomViewGradeReqs__Staff gcFamMem (gc, gcOld)
      viewGradesStuds <- randomViewGradeReqs__Student gcFamMem (gc, gcOld)
      return $
        postAssigns ++ editAssigns ++ postGrades ++ viewGradesStaff ++ viewGradesStuds
  | gcFamMem <- gcFam
  | gc <- gcPool
  | gcOld <- emptyGClassroom:(gcPool & init)
  ] & sequence
  where
    gcPool = gcFam & gcFamilyToPool

    -- cardinality
    cardPostAssignmentActualPriv :: GClassroom -> (Int,Int)
    cardPostAssignmentActualPriv gc =
      -- ideal privilege cardinality
      let ic =
            [ (teacher, assignment)
            | teacher <- gc & teachers
            , assignment <- teacher & getAssignments gc
            ] & length
      -- over privilege cardinality
          oc =
            [ (ta, assignment)
            | ta <- gc & tas
            , assignment <- ta & getAssignments gc
            ] & length
      in
        (ic, oc)

    cardEditAssignmentActualPriv :: GClassroom -> (Int,Int)
    cardEditAssignmentActualPriv = cardPostAssignmentActualPriv

    cardPostGradeActualPriv :: GClassroom -> (Int,Int)
    cardPostGradeActualPriv gc =
      let ic =
            [ (staffMem, grade)
            | staffMem <- gc & getAllStaff
            , grade <- staffMem & getGrades gc
            ] & length
          oc = 0
      in
        (ic, oc)

    cardViewGradeActualPriv__Staff :: GClassroom -> (Int,Int)
    cardViewGradeActualPriv__Staff gc =
      let ic =
            [ (staffMem, grade)
            | staffMem <- gc & getAllStaff
            , grade <- staffMem & getGrades gc
            ] & length
          oc =
            [ (staffMem, grade)
            | staffMem <- gc & getAllStaff
            , grade <- staffMem & getGradesNotForStaffMember gc
            ] & length
      in
        (ic, oc)

    cardViewGradeActualPriv__Student :: GClassroom -> (Int,Int)
    cardViewGradeActualPriv__Student gc =
      let ic =
            [ (stud, grade)
            | stud <- gc & students
            , grade <- stud & getStudentGrades gc
            ] & length
          oc = 0
      in (ic, oc)

    -- requests
    randomPostAssignmentReqs ::
      RandomGen g => GClassroom -> (GClassroom,GClassroom) -> State g [Request']
    randomPostAssignmentReqs gcDelta (gc, gcOld) = do
      let
        (idealNumDelta, overNumDelta) =
          ( conf & privilegeRepresentation (gc    & cardPostAssignmentActualPriv)
          , conf & privilegeRepresentation (gcOld & cardPostAssignmentActualPriv)
          ) & uncurry privilegeRepresentationDelta
      idealRep <- replicateM idealNumDelta $ pPostAssignment teachers (gcDelta,gc) conf
      overRep  <- replicateM overNumDelta  $ pPostAssignment tas (gcDelta,gc) conf
      return $ idealRep ++ overRep


    randomEditAssignmentReqs ::
      RandomGen g => GClassroom -> (GClassroom,GClassroom) -> State g [Request']
    randomEditAssignmentReqs gcDelta (gc, gcOld) = do
      tes' <- conf & samplePrincipals teachers (gcDelta, gc)
      tas' <- conf & samplePrincipals tas (gcDelta, gc)
      let
        (idealNumDelta, overNumDelta) =
          ( conf & privilegeRepresentation (gc    & cardEditAssignmentActualPriv)
          , conf & privilegeRepresentation (gcOld & cardEditAssignmentActualPriv))
          & uncurry privilegeRepresentationDelta
      idealRep <- replicateM idealNumDelta $ pEditAssignment teachers (gcDelta, gc) conf
      overRep  <- replicateM overNumDelta  $ pEditAssignment tas (gcDelta, gc) conf
      return $ idealRep ++ overRep

    randomPostGradeReqs ::
      RandomGen g => GClassroom -> (GClassroom,GClassroom) -> State g [Request']
    randomPostGradeReqs gcDelta (gc, gcOld) = do
      -- staff' <- conf & samplePrincipals getAllStaff (gcDelta, gc)
      let
        (idealNum, _) =
          ( conf & privilegeRepresentation (gc    & cardPostGradeActualPriv)
          , conf & privilegeRepresentation (gcOld & cardPostGradeActualPriv))
          & uncurry privilegeRepresentationDelta
      replicateM idealNum $ pPostGrade getAllStaff (gcDelta, gc) conf

    randomViewGradeReqs__Staff ::
      RandomGen g => GClassroom -> (GClassroom,GClassroom) -> State g [Request']
    randomViewGradeReqs__Staff gcDelta (gc, gcOld) = do
      let
        (idealNum, overNum) =
          ( conf & privilegeRepresentation (gc    & cardViewGradeActualPriv__Staff)
          , conf & privilegeRepresentation (gcOld & cardViewGradeActualPriv__Staff))
          & uncurry privilegeRepresentationDelta
      idealRep <- replicateM idealNum $ ipViewGrade__Staff getAllStaff (gcDelta, gc) conf
      overRep  <- replicateM overNum  $ opViewGrade__Staff (selectStaff gc) (gcDelta, gc) conf
      return $ idealRep ++ overRep
      where
        selectStaff :: GClassroom -> GClassroom -> [(Staff,[Grade])]
        selectStaff gcPool gc' =
          gc'
          & getAllStaff
          & map (\ s -> (s, s & getGradesNotForStaffMember gcPool))
          & filter (\ (s, grs) -> not (null grs))

    -- no over privilege
    randomViewGradeReqs__Student ::
      RandomGen g => GClassroom -> (GClassroom,GClassroom) -> State g [Request']
    randomViewGradeReqs__Student gcDelta (gc, gcOld) = do
      let
        idealNum =
          ( conf & privilegeRepresentation (gc    & cardViewGradeActualPriv__Student)
          , conf & privilegeRepresentation (gcOld & cardViewGradeActualPriv__Student))
          & uncurry privilegeRepresentationDelta
          & uncurry (+)
      replicateM idealNum $ pViewGrade__Student students (gcDelta, gc) conf

{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module Lib.HotCRP.Types where

import Data.Aeson
import Data.Function
import GHC.Generics

import Lib.Action
import Lib.Entity
import Lib.Request

type Area = Entity Value
mkArea :: String -> Area
mkArea name = Entity (mkUID "Area" name) (object []) []

data UserAttrs
  = UserAttrs
    { isPCChair :: Bool
    , isAreaChair :: Bool
    , pcMember :: Maybe UID}
  deriving (Generic, Show)

instance ToJSON UserAttrs where
  toJSON UserAttrs{..} = object $
       ["isPCChair" .= isPCChair, "isAreaChair" .= isAreaChair]
    ++ (pcMember & maybe [] (\ area -> ["pcMember"  .= area]))

type User = Entity UserAttrs
mkUser :: String -> Bool -> Bool -> Maybe Area -> User
mkUser name isPCChair isAreaChair pcMember =
  let pcMember'  = uid <$> pcMember in
    Entity (mkUID "User" name) (UserAttrs isPCChair isAreaChair pcMember') []

data PaperAttrs
  = PaperAttrs
    { authors   :: [UID]
    , reviewers :: [UID]
    , area      :: UID
    }
  deriving (Generic, Show)

instance ToJSON PaperAttrs where

type Paper = Entity PaperAttrs
mkPaper :: String -> [User] -> [User] -> Area -> Paper
mkPaper name authors reviewers area =
  let authors'   = uid <$> authors
      reviewers' = uid <$> reviewers
      area'      = uid area
  in
  Entity
    (mkUID "Paper" name)
    (PaperAttrs authors' reviewers' area')
    []

data ReviewAttrs =
  ReviewAttrs
  { ofPaper :: UID
  , author  :: UID
  , isMetaReview :: Bool
  }
  deriving (Generic, Show)
instance ToJSON ReviewAttrs where

type Review = Entity ReviewAttrs
mkReview :: String -> Paper -> User -> Bool -> Review
mkReview name ofPaper author isMetaReview =
  let ofPaper' = uid ofPaper
      author'  = uid author
  in
  Entity
    (mkUID "Review" name)
    (ReviewAttrs ofPaper' author' isMetaReview)
    []

data HotCRPCtxt =
  HotCRPCtxt { isReleased :: Maybe Bool  }
  deriving (Generic, Show)

instance ToJSON HotCRPCtxt where
  toJSON HotCRPCtxt{..} = object $
    (isReleased & maybe [] (\ ir -> ["isReleased" .= ir]))

type HotCRPRequest = Request HotCRPCtxt

data HotCRP = HotCRP
  { mereAuthors      :: [User]
  , programCommittee :: [User]
  , areas            :: [Area]
  , papers           :: [Paper]
  , reviews          :: [Review]
  }

emptyHotCRP :: HotCRP
emptyHotCRP =
  HotCRP
  { mereAuthors = []
  , programCommittee = []
  , areas = []
  , papers = []
  , reviews = []
  }

mergeHotCRP :: HotCRP -> HotCRP -> HotCRP
mergeHotCRP hc1 hc2 =
  HotCRP
  { mereAuthors = (hc1 & mereAuthors) ++ (hc2 & mereAuthors)
  , programCommittee = (hc1 & programCommittee) ++ (hc2 & programCommittee)
  , areas = (hc1 & areas) ++ (hc2 & areas)
  , papers = (hc1 & papers) ++ (hc2 & papers)
  , reviews = (hc1 & reviews) ++ (hc2 & reviews)
  }

data HotCRPAction =
  ReleaseReview
  | Read
  | Update
  | Delete
  deriving (Show)

toAction :: HotCRPAction -> Action
toAction = Action . show

readPaper :: User -> Paper -> HotCRPRequest
readPaper u p = Read & toAction & toRequest u p (HotCRPCtxt { isReleased = Nothing })

readReview :: User -> Review -> HotCRPCtxt -> HotCRPRequest
readReview u p c = Read & toAction & toRequest u p c

data HotCRPEntity =
  HotCRPUser User
  | HotCRPArea Area
  | HotCRPPaper Paper
  | HotCRPReview Review
  deriving (Generic, Show)
instance ToJSON HotCRPEntity where
  toJSON (HotCRPUser user) = toJSON user
  toJSON (HotCRPArea area) = toJSON area
  toJSON (HotCRPPaper pap) = toJSON pap
  toJSON (HotCRPReview rv) = toJSON rv


toHCEntities :: HotCRP -> [HotCRPEntity]
toHCEntities (HotCRP {..}) =
     map HotCRPArea areas
  ++ map HotCRPUser (programCommittee ++ mereAuthors)
  ++ map HotCRPPaper papers
  ++ map HotCRPReview reviews

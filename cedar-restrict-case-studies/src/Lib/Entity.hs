{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}

module Lib.Entity where

import Control.Arrow
import Data.Aeson
import Data.Function
import GHC.Generics

import Lib.CedarFormat

data UIDData = UIDData { _type :: String , _id :: String }
  deriving (Generic, Show, Eq)
instance ToJSON UIDData where
  toJSON (UIDData _type _id) =
    object ["id" .= _id, "type" .= _type ]

instance CedarFormat UIDData where
  cedarFormat UIDData{..} =
    show $ _type ++ "::" ++ show _id

data UID = UID { __entity :: UIDData }
  deriving (Generic, Show, Eq)
instance ToJSON UID

instance CedarFormat UID where
  cedarFormat (UID d) = cedarFormat d

mkUID :: String -> String -> UID
mkUID _type name =
  UID (UIDData _type name)

data Entity a = Entity { uid :: UID, attrs :: a, parents :: [UID] }
  deriving (Generic, Show, Eq)
instance ToJSON a => ToJSON (Entity a)

getEntityName :: Entity a -> String
getEntityName e = e & (uid >>> __entity >>> _id)

instance CedarFormat (Entity a) where
  cedarFormat (Entity uid _ _) = cedarFormat uid

modifyAtUID :: UID -> (Entity a -> Entity a) -> [Entity a] -> [Entity a]
modifyAtUID u f [] = []
modifyAtUID u f (e:es)
  | u == uid e = f e : es
  | otherwise  = e : modifyAtUID u f es

addParent :: UID -> Entity a -> Entity a
addParent u e = e { parents = u : parents e }

entityElem :: Entity a -> [UID] -> Bool
entityElem e uids = (e & uid) `elem` uids

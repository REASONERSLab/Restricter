{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Lib.Request where

import Data.Aeson
import GHC.Generics

import Lib.Entity
import Lib.Action

data Request c =
  Request
  { principal :: UIDData
  , action    :: Action
  , resource  :: UIDData
  , context   :: c
  } deriving (Generic, Show)

mkRequest :: Entity p -> Action -> Entity r -> c -> Request c
mkRequest
  (Entity (UID p_uid_data) _ _)
  act
  (Entity (UID r_uid_data) _ _)
  ctxt =
  Request p_uid_data act r_uid_data ctxt

type Request' = Request Value

mkRequest' :: Entity p -> Action -> Entity r -> Request'
mkRequest' p a r = mkRequest p a r (object [])

toRequest :: Entity p -> Entity r -> c -> Action -> Request c
toRequest p r c a = mkRequest p a r c

toRequest' :: Entity p -> Entity r -> Action -> Request'
toRequest' p r a = mkRequest' p a r

instance ToJSON c => ToJSON (Request c) where
  toJSON
    (Request
       (UIDData p_type p_name)
       (Action act)
       (UIDData r_type r_name)
       ctxt) =
    object
      [ "principal" .= (p_type ++ "::" ++ show p_name)
      , "action"    .= ("Action::" ++ show act)
      , "resource"  .= (r_type ++ "::" ++ show r_name)
      , "context"   .= ctxt ]

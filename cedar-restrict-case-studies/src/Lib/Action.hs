module Lib.Action where

import Lib.CedarFormat

newtype Action = Action String
  deriving (Show)

instance CedarFormat Action where
  cedarFormat (Action name) =
    show $ "Action::" ++ show name

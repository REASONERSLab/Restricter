module Lib.CedarFormat where

class CedarFormat a where
  cedarFormat :: a -> String

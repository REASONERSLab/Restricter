{-# LANGUAGE DeriveGeneric, RecordWildCards, OverloadedStrings #-}

module Lib.IO where

import Data.Aeson

import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Char
import Data.Function
import GHC.Generics
import System.Process.Typed

import Lib.CedarFormat
import Lib.Request

data CedarCtxt =
  CedarCtxt
  { exe :: FilePath
  , schema :: Maybe FilePath
  , entities :: FilePath
  , policies :: FilePath
  }

authorize :: CedarCtxt -> Request c -> (c -> Maybe FilePath) -> IO String
authorize CedarCtxt{..} Request{..} ctxtHandler = do
  (_, out, err) <- readProcess cmd
  return (out & B.toString & filter (not . isSpace))
  where
    cmd :: ProcessConfig () () ()
    cmd = shell $
         exe ++ " authorize "
      ++ "--principal " ++ cedarFormat principal ++ " "
      ++ "--action "    ++ cedarFormat action    ++ " "
      ++ "--resource "  ++ cedarFormat resource  ++ " "
      ++ (case (ctxtHandler context) of
            Nothing -> ""
            Just fp -> "--context " ++ fp ++ " ")
      ++ "--policies "  ++ policies              ++ " "
      ++ (case schema of
            Nothing -> ""
            Just sch -> "--schema " ++ sch ++ " ")
      ++ "--entities " ++ entities

authorize' :: CedarCtxt -> Request' -> IO String
authorize' cedarCtxt req = authorize cedarCtxt req (const Nothing)

data LogEntry c =
  LogEntry
    { request :: Request c
    , result  :: String
    } deriving (Generic, Show)

type LogEntry' = LogEntry Value

instance ToJSON c => ToJSON (LogEntry c) where

{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import           Control.Concurrent.STM
import           Protolude

type Id         = Int
type AttName    = Text
type AttValue   = Text
type DBContents = Map Id (Map AttName AttValue)
type DB         = TVar DBContents




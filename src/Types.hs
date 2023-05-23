{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import           Control.Concurrent.STM
import           Protolude

type Id       = Int
type AttName  = Text
type AttValue = Text
type DB       = TVar (Map Int (Map Text Text))




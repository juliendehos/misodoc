{-# LANGUAGE OverloadedStrings #-}

module Model where

import Miso
import Miso.Lens
import Miso.Lens.TH

import Helpers

data MyError
  = FetchError MisoString MisoString
  | ParseError MisoString
  deriving (Eq)

data Model = Model
  { _modelError :: Maybe MyError
  , _modelChapters :: [MisoString]
  , _modelCurrent :: MisoString
  , _modelSummary :: [Node]
  , _modelPage :: [Node]
  , _modelDebug :: Bool
  } deriving (Eq)

makeLenses ''Model

mkModel :: Model
mkModel = Model Nothing [] "" [] [] False


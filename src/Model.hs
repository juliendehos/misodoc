{-# LANGUAGE OverloadedStrings #-}

module Model where

import Miso
import Miso.Lens
import Miso.Lens.TH

import Helpers

data Model = Model
  { _modelError :: MisoString
  , _modelChapters :: [MisoString]
  , _modelCurrent :: MisoString
  , _modelSummary :: [Node]
  , _modelPage :: [Node]
  , _modelDebug :: Bool
  } deriving (Eq)

makeLenses ''Model

mkModel :: Model
mkModel = Model "" [] "" [] [] False


{-# LANGUAGE OverloadedStrings #-}

module Model where

import Miso
import Miso.Lens
import Miso.Lens.TH

data Model = Model
  { _modelError :: MisoString
  , _modelSummary :: MisoString
  , _modelPage :: MisoString
  } deriving (Eq)

makeLenses ''Model

mkModel :: Model
mkModel = Model "" "" ""


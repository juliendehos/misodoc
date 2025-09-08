{-# LANGUAGE OverloadedStrings #-}

module Model where

import CMark
import Miso
import Miso.Lens
import Miso.Lens.TH
import Miso.Html.Element as H

data Model = Model
  { _modelError :: MisoString
  , _modelChapters :: [MisoString]
  , _modelCurrent :: MisoString
  , _modelSummary :: Node
  , _modelPage :: Node
  , _modelDebug :: Bool
  } deriving (Eq)

makeLenses ''Model

mkModel :: Model
mkModel = Model "" [] "" emptyNode emptyNode False

emptyNode :: Node
emptyNode = Node Nothing DOCUMENT []


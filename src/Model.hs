{-# LANGUAGE OverloadedStrings #-}

module Model where

import CMark
import Miso
import Miso.Lens
import Miso.Lens.TH
import Miso.Html.Element as H

import Action

data Model = Model
  { _modelError :: MisoString
  , _modelChapters :: [MisoString]
  , _modelSummary :: Node
  , _modelPage :: Node
  } deriving (Eq)

makeLenses ''Model

mkModel :: Model
mkModel = Model "" [] emptyNode emptyNode

emptyNode :: Node
emptyNode = Node Nothing DOCUMENT []


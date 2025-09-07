{-# LANGUAGE OverloadedStrings #-}

module Model where

import Miso
import Miso.Lens
import Miso.Lens.TH
import Miso.Html.Element as H

import Action

instance Eq (View m a)  where
  VNode ns1 str1 _ n1 == VNode ns2 str2 _ n2 = ns1 == ns2 && str1 == str2 && n1 == n2
  VText str1 == VText str2 = str1 == str2
  VComp ns1 str1 _ _ == VComp ns2 str2 _ _ = ns1 == ns2 && str1 == str2
  _ == _ = False

data Model = Model
  { _modelError :: MisoString
  , _modelChapters :: [MisoString]
  , _modelSummary :: View Model Action
  , _modelPage :: View Model Action
  } deriving (Eq)

makeLenses ''Model

mkModel :: Model
mkModel = Model "" [] emptyView emptyView

emptyView :: View Model Action
emptyView = div_ [] []

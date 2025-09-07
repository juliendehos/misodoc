{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Component (mkComponent) where

import Miso
import Miso.CSS as CSS
import Miso.Lens
import Miso.Html.Element as H
-- import Miso.Html.Event as E
-- import Miso.Html.Property as P
-- import Miso.String qualified as MS

import Action
import Helpers
import Model

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Transition Model Action

updateModel (ActionError str) =
  modelError .= str

updateModel (ActionAskMd fp) =
  getText fp [] ActionSetMd ActionError

updateModel (ActionSetMd str) =
  modelPage .= renderNode str

updateModel (ActionAskSummary fp) =
  getText fp [] ActionSetSummary ActionError

updateModel (ActionSetSummary str) = do
  let node' = renderNode str
  modelSummary .= node'
  modelChapters .= parseChapters node'

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel m =
  div_ [ CSS.style_ [ CSS.display "flex", CSS.flexDirection "row" ] ]
    [ viewSummary m
    , viewPage m
    ]

viewSummary :: Model -> View Model Action
viewSummary Model{..} = 
  div_ 
    [ CSS.style_ [ CSS.paddingRight "20px", minWidth "300px", maxWidth "300px" ] ]
    [ h2_ [] [ "Summary" ]
    , renderSummary _modelSummary
    , p_ [] [ text _modelError ]
    -- , p_ [] [ ul_ [] (fmap (\u -> li_ [] [ text u]) _modelChapters) ]
    ]

viewPage :: Model -> View Model Action
viewPage Model{..} = 
  div_ [] ( renderPage _modelPage : viewRaw )
  where
    viewRaw
      | _modelPage == emptyNode = []
      | otherwise = 
          [ hr_ []
          , p_ [] [ "CMark Node:" ]
          , p_ [] [ renderRaw _modelPage ]
          ]

-------------------------------------------------------------------------------
-- component
-------------------------------------------------------------------------------

mkComponent :: App Model Action
mkComponent = 
  (component mkModel updateModel viewModel)
    { initialAction = Just (ActionAskSummary "summary.md") }


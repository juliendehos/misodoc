{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Component (mkComponent) where

import Miso
import Miso.CSS as CSS
import Miso.Lens
import Miso.Html.Element as H
import Miso.Html.Event as E
-- import Miso.Html.Property as P
import Miso.String qualified as MS

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
  modelPage .= str

updateModel (ActionAskSummary fp) =
  getText fp [] ActionSetSummary ActionError

updateModel (ActionSetSummary str) =
  modelSummary .= str

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
  div_ [ CSS.style_ [ CSS.paddingRight "20px", width "300px" ] ]
    ([ h2_ [] [ "Summary" ]
    -- , p_ [] [ button_ [ onClick (ActionAskMd "page2.md") ] [ "fetch page 2" ] ]
    , p_ [] [ text _modelError ]
    ] ++ [ renderSummary _modelSummary, hr_ [], p_ [] [ renderRaw _modelSummary] ] )

viewPage :: Model -> View Model Action
viewPage Model{..} = div_ [] fmtPage
  where
    fmtPage
      | MS.null _modelPage = []
      | otherwise =
          [ renderPage _modelPage
          , hr_ []
          , p_ [] [ renderRaw _modelPage ]
          ]

-------------------------------------------------------------------------------
-- component
-------------------------------------------------------------------------------

mkComponent :: App Model Action
mkComponent = 
  (component mkModel updateModel viewModel)
    { initialAction = Just (ActionAskSummary "summary.md") }


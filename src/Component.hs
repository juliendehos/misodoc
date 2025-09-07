{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Component (mkComponent) where

import Miso
import Miso.CSS as CSS
import Miso.Lens
import Miso.Html.Element as H
-- import Miso.Html.Event as E
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
  modelPage .= 
    div_ 
      []
      [ renderPage str
      , hr_ []
      , p_ [] [ "MD Tree:" ]
      , p_ [] [ renderRaw str ]
      ]

updateModel (ActionAskSummary fp) =
  getText fp [] ActionSetSummary ActionError

updateModel (ActionSetSummary str) = do
  err <- use modelError
  modelSummary .= 
    div_ 
      [ CSS.style_ [ CSS.paddingRight "20px", minWidth "300px", maxWidth "300px" ] ]
      [ h2_ [] [ "Summary" ]
      , renderSummary str
      , p_ [] [ text err ]
      ]

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel Model{..} =
  div_ [ CSS.style_ [ CSS.display "flex", CSS.flexDirection "row" ] ]
    [ _modelSummary
    , _modelPage
    ]

-------------------------------------------------------------------------------
-- component
-------------------------------------------------------------------------------

mkComponent :: App Model Action
mkComponent = 
  (component mkModel updateModel viewModel)
    { initialAction = Just (ActionAskSummary "summary.md") }


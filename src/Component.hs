{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Component (mkComponent) where

import Miso
import Miso.CSS as CSS
import Miso.Lens
import Miso.Html.Element as H
import Miso.Html.Event as E
-- import Miso.Html.Property as P

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

updateModel (ActionSetMd str) = do
  modelPage .= renderNode str
  modelError .= ""

updateModel (ActionAskSummary fp) =
  getText fp [] ActionSetSummary ActionError

updateModel (ActionSetSummary str) = do
  let node' = renderNode str
  modelSummary .= node'
  modelChapters .= parseChapters node'
  modelError .= ""

updateModel ActionSwitchDebug =
  modelDebug %= not

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
    (
      [ h2_ [] [ "Summary" ]
      , p_ [] [ text _modelError ]
      , renderSummary _modelSummary
      ] ++ fmtDebug
    )
  where
    fmtDebug =
      if _modelDebug
        then
          [ p_ [] [ button_ 
                      [ onClick ActionSwitchDebug ] 
                      [ "Switch off Debug" ] ]
          , hr_ []
          , p_ []
              [ "chapter links:"
              , ul_ [] (fmap (\u -> li_ [] [ text u]) _modelChapters)
              ]
          ]
        else
          [ p_ [] [ button_ 
                      [ onClick ActionSwitchDebug ] 
                      [ "Switch on Debug" ] ]
          ]

viewPage :: Model -> View Model Action
viewPage Model{..} = 
  div_ [] ( renderPage _modelChapters _modelPage : viewRaw )
  where
    viewRaw
      | _modelPage == emptyNode = []
      | _modelDebug = 
          [ hr_ []
          , p_ [] [ renderRaw _modelPage ]
          , hr_ []
          , p_ [] [ renderPretty _modelPage ]
          ]
      | otherwise = []

-------------------------------------------------------------------------------
-- component
-------------------------------------------------------------------------------

mkComponent :: App Model Action
mkComponent = 
  (component mkModel updateModel viewModel)
    { initialAction = Just (ActionAskSummary "summary.md") }


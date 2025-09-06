{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Component (mkComponent) where

import Miso
import Miso.CSS as CSS
import Miso.Lens
import Miso.Html.Element as H
import Miso.Html.Event as E
-- import Miso.Html.Property as P
import Miso.String as MS

import Helpers
import Model

-------------------------------------------------------------------------------
-- actions
-------------------------------------------------------------------------------

data Action
  = ActionError MisoString
  | ActionAskMd MisoString
  | ActionSetMd MisoString
  | ActionAskSummary MisoString
  | ActionSetSummary MisoString

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
  pure ()
  -- TODO
  -- getText fp [] ActionSetMd ActionError

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
viewSummary m = 
  div_ [ CSS.style_ [ CSS.paddingRight "20px" ] ]
    [ h2_ [] [ "Summary" ]
    , p_ [] [ "foo bar" ]
    ]

viewPage :: Model -> View Model Action
viewPage Model{..} = 
  div_ []
    ([ h2_ [] [ "the page" ]
    , p_ [] [ text _modelError ]
    , p_ [] [ a_ 
                [ onClick (ActionAskMd "page1.md")
                , CSS.style_ 
                  [ CSS.textDecoration "underline blue"
                  , CSS.color CSS.blue
                  , CSS.cursor "pointer" 
                  ]
                ]
                [ "fetch page 1" ]
            ]
    , p_ [] [ button_ [ onClick (ActionAskMd "page1.md") ] [ "fetch page 1" ] ]
    , p_ [] [ button_ [ onClick (ActionAskMd "page2.md") ] [ "fetch page 2" ] ]
    ] ++ fmtPage)

  where
    fmtPage
      | MS.null _modelPage = []
      | otherwise =
          [ hr_ []
          , renderMd _modelPage
          , hr_ []
          , p_ [] [ renderRaw _modelPage ]
          , hr_ []
          ]

-------------------------------------------------------------------------------
-- component
-------------------------------------------------------------------------------

mkComponent :: App Model Action
mkComponent = 
  (component mkModel updateModel viewModel)
    { initialAction = Just (ActionAskSummary "summary.md") }


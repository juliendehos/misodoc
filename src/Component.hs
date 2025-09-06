{-# LANGUAGE OverloadedStrings #-}

module Component (mkComponent) where

import Miso
import Miso.Lens
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P
import Miso.CSS as CSS

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

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel m = 
  div_ []
    [ p_ [] [ text (m ^. modelError) ]
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
    , renderMd (m ^. modelPage)
    , p_ [] [ renderRaw (m ^. modelPage) ]
    ]

-------------------------------------------------------------------------------
-- component
-------------------------------------------------------------------------------

mkComponent :: App Model Action
mkComponent = 
  (component mkModel updateModel viewModel)
    { initialAction = Just (ActionAskSummary "summary.md") }


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Component (mkComponent) where

import Miso
import Miso.CSS qualified as CSS
import Miso.Lens
import Miso.Html.Element as H
import Miso.Html.Event as E

import Helpers
import Model

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

data Action
  = ActionError MisoString
  | ActionAskPage MisoString
  | ActionSetPage MisoString MisoString
  | ActionAskSummary MisoString
  | ActionSetSummary MisoString MisoString
  | ActionSwitchDebug

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Transition Model Action

updateModel (ActionError str) =
  modelError .= str

updateModel (ActionAskPage fp) =
  getText fp [] (ActionSetPage fp) ActionError

updateModel (ActionSetPage fp str) = do
  case parseNodes fp str of
    Left err -> modelError .= err
    Right ns -> do
      modelCurrent .= fp
      modelPage .= ns
      modelError .= ""

updateModel (ActionAskSummary fp) =
  getText fp [] (ActionSetSummary fp) ActionError

updateModel (ActionSetSummary fp str) = do
  case parseNodes fp str of
    Left err -> modelError .= err
    Right ns -> do
      modelSummary .= ns
      modelError .= ""
      case getChapters ns of
        [] -> pure ()
        chapters@(c:_) -> do
          modelChapters .= chapters
          issue $ ActionAskPage c

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
    [ CSS.style_ 
        [ CSS.paddingRight "20px"
        , CSS.minWidth "300px"
        , CSS.maxWidth "300px" ]
        ]
    (
      [ h2_ [] [ "Summary" ]
      -- , renderSummary formatters _modelSummary
      , p_ [] [ text _modelError ]
      ] ++ fmtDebug
    )
  where
    fmtDebug =
      if _modelDebug
        then
          [ p_ [] [ button_ 
                      [ onClick ActionSwitchDebug ] 
                      [ "Hide Debug" ] ]
          , hr_ []
          , p_ []
              [ "chapter links:"
              , ul_ [] (fmap (\u -> li_ [] [ text u]) _modelChapters)
              , text ("current: " <> _modelCurrent)
              ]
          ]
        else
          [ p_ [] [ button_ 
                      [ onClick ActionSwitchDebug ] 
                      [ "Show Debug" ] ]
          ]

viewPage :: Model -> View Model Action
viewPage Model{..} = 
  div_ [] 
    (
      [ viewNav
      -- , renderPage formatters _modelChapters _modelPage
      ] ++ viewRaw
    )
  where
    viewNav = case getPreviousNext _modelChapters _modelCurrent of
      (Just prev, Just next) -> 
        p_ [] 
          [ fmtChapterLink formatters prev ["previous"]
          , " - "
          , fmtChapterLink formatters next ["next"]
          ]
      (Nothing, Just next) -> p_ [] [ "previous - ", fmtChapterLink formatters next ["next"] ]
      (Just prev, Nothing) -> p_ [] [ fmtChapterLink formatters prev ["previous"], " - next" ]
      _ -> div_ [] []

    viewRaw
      | null _modelPage = []
      | _modelDebug = 
          [ hr_ []
          , p_ [] [ renderRaw _modelPage ]
          , hr_ []
          -- , p_ [] [ renderPretty _modelPage ]
          ]
      | otherwise = []

formatters :: Formatters Model Action
formatters = Formatters
  { fmtChapterLink = \u ns -> 
      a_ 
        [ onClick (ActionAskPage (ms u))
        , CSS.style_ 
          [ CSS.textDecoration "underline blue"
          , CSS.color CSS.blue
          , CSS.cursor "pointer" 
          ]
        ]
      ns
  , fmtInlineCode = \t ->
      span_ [ CSS.style_ [ CSS.backgroundColor CSS.lightgrey ] ] [ text t ]
  , fmtBlockQuote = 
      pre_ 
        [ CSS.style_ 
          [ CSS.border "1px solid black"
          , CSS.padding "10px"
          , CSS.backgroundColor CSS.lightyellow
          ]
        ]
  , fmtCodeBlock = 
      pre_ 
        [ CSS.style_ 
          [ CSS.border "1px solid black"
          , CSS.padding "10px"
          , CSS.backgroundColor CSS.lightgrey
          ]
        ]
  }

-------------------------------------------------------------------------------
-- component
-------------------------------------------------------------------------------

mkComponent :: App Model Action
mkComponent = 
  (component mkModel updateModel viewModel)
    { initialAction = Just (ActionAskSummary "summary.md") }


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Component (mkComponent) where

import Data.Maybe (isNothing)
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
  = ActionFetchError MisoString MisoString
  | ActionAskPage MisoString
  | ActionSetPage MisoString MisoString
  | ActionAskSummary MisoString
  | ActionSetSummary MisoString MisoString
  | ActionSwitchDebug

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Transition Model Action

updateModel (ActionFetchError fp str) = do
  modelError ?= FetchError fp str
  modelCurrent .= ""

updateModel (ActionAskPage fp) =
  getText fp [] (ActionSetPage fp) (ActionFetchError fp)

updateModel (ActionSetPage fp str) = do
  modelCurrent .= fp
  case parseNodes fp str of
    Left err -> modelError ?= ParseError err
    Right ns -> do
      modelPage .= ns
      modelError .= Nothing

updateModel (ActionAskSummary fp) =
  getText fp [] (ActionSetSummary fp) (ActionFetchError fp)

updateModel (ActionSetSummary fp str) = do
  case parseNodes fp str of
    Left err -> modelError ?= ParseError err
    Right ns -> do
      modelSummary .= ns
      modelError .= Nothing
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
viewModel m@Model{..} =
  div_ [ CSS.style_ [ CSS.display "flex", CSS.flexDirection "row" ] ]
    [ viewSummary m
    , if isNothing _modelError then viewPage m else viewError m
    ]

viewSummary :: Model -> View Model Action
viewSummary Model{..} = 
  div_ 
    [ CSS.style_ 
        [ CSS.paddingRight "20px"
        , CSS.minWidth "300px"
        , CSS.maxWidth "300px" ]
        ]
    [ h2_ [] [ "MisoDoc" ]
    , renderNodes formatters _modelChapters _modelSummary
    , viewDebug
    ]
  where
    viewDebug = 
      div_ [] $ if _modelDebug
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
viewPage m@Model{..} = 
  div_ [] 
    [ viewNav m
    , renderNodes formatters _modelChapters _modelPage
    , viewDebug m
    ]
  where
    viewDebug Model{..} = 
      div_ [] $ if not _modelDebug || null _modelPage 
        then
          []
        else 
          [ hr_ []
          , p_ [] [ renderRaw _modelPage ]
          ]

viewError :: Model -> View Model Action
viewError Model{..} = 
  div_ [] 
      [ h2_ [] [ text errorType ]
      , pre_ 
          [ CSS.style_ 
              [ CSS.backgroundColor CSS.lightpink
              , CSS.padding "20px"
              , CSS.border "1px solid black"
              ]
          ]
          [ text errorMsg ]
      ]
  where
    (errorType, errorMsg) = case _modelError of
      Just (FetchError fp msg) -> ("Fetch error: " <> fp, msg)
      Just (ParseError msg) -> ("Parse error", msg)
      Nothing -> ("no error", "no error")

viewNav :: Model -> View Model Action
viewNav Model{..} = 
  case getPreviousNext _modelChapters _modelCurrent of
    (Just prev, Just next) -> 
      p_ [] 
        [ _fmtChapterLink formatters prev ["previous"]
        , " - "
        , _fmtChapterLink formatters next ["next"]
        ]
    (Nothing, Just next) -> p_ [] [ "previous - ", _fmtChapterLink formatters next ["next"] ]
    (Just prev, Nothing) -> p_ [] [ _fmtChapterLink formatters prev ["previous"], " - next" ]
    _ -> div_ [] []

formatters :: Formatters Model Action
formatters = Formatters
  { _fmtChapterLink = \u ns -> 
      a_ 
        [ onClick (ActionAskPage (ms u))
        , CSS.style_ 
          [ CSS.textDecoration "underline blue"
          , CSS.color CSS.blue
          , CSS.cursor "pointer" 
          ]
        ]
      ns
  -- TODO
  , _fmtInlineCode = \t ->
      code_ [ CSS.style_ [ CSS.backgroundColor CSS.lightgrey ] ] [ text t ]
  , _fmtBlockQuote = 
      pre_ 
        [ CSS.style_ 
          [ CSS.border "1px solid black"
          , CSS.padding "10px"
          , CSS.backgroundColor CSS.lightyellow
          ]
        ]
  , _fmtCodeBlock = \str ->
      pre_ 
        [ CSS.style_ 
          [ CSS.border "1px solid black"
          , CSS.padding "10px"
          , CSS.backgroundColor CSS.lightgrey
          ]
        ]
        [ text str ]
  }

-------------------------------------------------------------------------------
-- component
-------------------------------------------------------------------------------

mkComponent :: App Model Action
mkComponent = 
  (component mkModel updateModel viewModel)
    { initialAction = Just (ActionAskSummary "summary.md") }


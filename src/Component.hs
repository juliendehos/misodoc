{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Component (mkComponent) where

import Data.Maybe (isNothing)
import Miso
import Miso.CSS qualified as CSS
import Miso.Lens
import Miso.Html.Element as H
import Miso.Html.Event as E

import Action
import FFI
import Markdown
import Model

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

headerNoCache :: (MisoString, MisoString)
headerNoCache = ("Cache-Control", "no-cache")

updateModel :: Action -> Transition Model Action

updateModel (ActionFetchError fp rep) = do
  let msg = ms ("errorMessage: " <> show (errorMessage rep) <> "\nbody: " <> show (body rep))
  modelError ?= FetchError fp msg
  modelCurrent .= ""

updateModel (ActionAskPage fp) =
  getText fp [headerNoCache] (ActionSetPage fp) (ActionFetchError fp)

updateModel (ActionSetPage fp rep) = do
  modelCurrent .= fp
  case parseNodes fp (body rep) of
    Left err -> modelError ?= ParseError err
    Right ns -> do
      modelPage .= ns
      modelError .= Nothing

updateModel (ActionAskSummary fp) =
  getText fp [headerNoCache] (ActionSetSummary fp) (ActionFetchError fp)

updateModel (ActionSetSummary fp rep) = do
  case parseNodes fp (body rep) of
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

updateModel (ActionRenderCode domref) = 
  io_ (renderCode domref)

updateModel (ActionRenderMath domref) =
  io_ (renderMath domref)

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
    , renderNodes formatter _modelChapters _modelSummary
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
          , hr_ []
          , p_ [] [ renderRaw _modelSummary ]
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
    , renderNodes formatter _modelChapters _modelPage
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
        [ _fmtChapterLink formatter prev ["previous"]
        , " - "
        , _fmtChapterLink formatter next ["next"]
        ]
    (Nothing, Just next) -> p_ [] [ "previous - ", _fmtChapterLink formatter next ["next"] ]
    (Just prev, Nothing) -> p_ [] [ _fmtChapterLink formatter prev ["previous"], " - next" ]
    _ -> div_ [] []

formatter :: Formatter Model Action
formatter = Formatter
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
  }

codeblockStyle :: CSS
codeblockStyle = Sheet $ CSS.sheet_
  [ CSS.selector_ "pre.codeblock"
    [ CSS.border "1px solid black"
    , CSS.padding "10px"
    , CSS.backgroundColor CSS.lightgrey
    ]
  ]

blockquoteStyle :: CSS
blockquoteStyle = Sheet $ CSS.sheet_
  [ CSS.selector_ "blockquote"
    [ CSS.border "1px solid black"
    , CSS.padding "10px"
    , CSS.backgroundColor CSS.lightyellow
    ]
  ]

tableStyle :: CSS
tableStyle = Sheet $ CSS.sheet_
  [ CSS.selector_ "table, th, td"
    [ CSS.border "1px solid black"
    , CSS.borderCollapse "collapse"
    ]
  , CSS.selector_ "th, td"
    [ CSS.paddingLeft "10px"
    , CSS.paddingRight "10px"
    ]
  ]

-------------------------------------------------------------------------------
-- component
-------------------------------------------------------------------------------

mkComponent :: App Model Action
mkComponent = 
  (component mkModel updateModel viewModel)
    { initialAction = Just (ActionAskSummary "summary.md")
    , styles = 
      [ Href "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.9.0/build/styles/default.min.css"
      , Href "https://cdn.jsdelivr.net/npm/katex@0.16.22/dist/katex.min.css"
      , codeblockStyle
      , blockquoteStyle
      , tableStyle
      ]
    , scripts = 
        [ Src "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.9.0/build/highlight.min.js"
        , Src "https://cdn.jsdelivr.net/npm/katex@0.16.22/dist/katex.min.js"
        ]
    }



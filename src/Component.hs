{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Component (mkComponent) where

import Data.Maybe (isNothing)
import Miso
import Miso.CSS qualified as CSS
import Miso.Lens
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P
import Text.Pandoc.Definition (MathType)

import FFI
import Markdown
import Model

-------------------------------------------------------------------------------
-- action
-------------------------------------------------------------------------------

data Action
  = ActionFetchError MisoString (Response MisoString)
  | ActionAskPage MisoString
  | ActionSetPage MisoString (Response MisoString)
  | ActionAskSummary MisoString
  | ActionSetSummary MisoString (Response MisoString)
  | ActionSwitchSummary
  | ActionSwitchDebug
  | ActionRenderCode DOMRef
  | ActionRenderMath MathType DOMRef
  | ActionScrollToTop

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
  io_ scrollToTop
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

updateModel ActionSwitchSummary =
  modelShowSummary %= not

updateModel ActionSwitchDebug =
  modelShowDebug %= not

updateModel (ActionRenderCode domref) = 
  io_ (renderCode domref)

updateModel (ActionRenderMath mathtype domref) =
  io_ (renderMath mathtype domref)

updateModel ActionScrollToTop =
  io_ scrollToTop

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel m@Model{..} =
  div_ [ CSS.style_ [ CSS.display "flex", CSS.flexDirection "row" ] ]
    [ if _modelShowSummary then viewSummary m else span_ [] []
    , if isNothing _modelError then viewPage m else viewError m
    ]

viewSummary :: Model -> View Model Action
viewSummary Model{..} = 
  div_ 
    [ CSS.style_ 
        [ CSS.paddingRight "10px"
        , CSS.minWidth "220px"
        , CSS.maxWidth "220px" ]
        ]
    [ renderNodes formatter _modelChapters _modelSummary
    , viewDebug
    ]
  where
    viewDebug = 
      div_ [] $ if _modelShowDebug
        then
          [ hr_ []
          , p_ []
              [ "chapter links:"
              , ul_ [] (fmap (\u -> li_ [] [ text u]) _modelChapters)
              , text ("current: " <> _modelCurrent)
              ]
          , hr_ []
          , p_ [] [ renderRaw _modelSummary ]
          ]
        else []

viewPage :: Model -> View Model Action
viewPage m@Model{..} = 
  div_ [] 
    [ viewTop
    , renderNodes formatter _modelChapters _modelPage
    , hr_ []
    , viewNav m
    , viewDebug
    ]
  where

    viewDebug = 
      div_ [] $ if _modelShowDebug && not (null _modelPage)
        then
          [ hr_ []
          , p_ [] [ renderRaw _modelPage ]
          ]
        else 
          []

    viewTop = 
      div_ []
        [ mkLink ActionSwitchSummary [ img_ [ src_ "icon-toc.jpg", height_ "20" ] ]
        , " "
        , mkLink ActionSwitchDebug [ img_ [ src_ "icon-bug.jpg", height_ "20" ] ]
        , span_ 
            [ CSS.style_ 
              [ CSS.fontWeight "bold"
              , CSS.fontSize "16pt"
              , CSS.paddingLeft "10px"
              ]
            ]
            [ text docTitle ]
        , hr_ []
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
  p_ [] 
    [ fmtImg "icon-left.jpg" "icon-left-ko.jpg" mPrev
    , " "
    , img_ [ src_ "icon-top.jpg", height_ "20", onClick ActionScrollToTop ]
    , " "
    , fmtImg "icon-right.jpg" "icon-right-ko.jpg" mNext
    ]
  where

    (mPrev, mNext) = getPreviousNext _modelChapters _modelCurrent

    fmtImg imgOk imgKo = \case
      Nothing -> img_ [ src_ imgKo, height_ "20" ]
      (Just x) -> img_ [ src_ imgOk, height_ "20", onClick (ActionAskPage x) ]


formatter :: Formatter Model Action
formatter = Formatter
  { _fmtChapterLink = mkLink . ActionAskPage . ms
  , _fmtCodeBlock = \langClass ns ->
      pre_ 
        [ class_ langClass
        , onCreatedWith_ ActionRenderCode 
        , CSS.style_
            [ CSS.border "1px solid black"
            , CSS.padding "10px"
            , CSS.backgroundColor #EEEEEE
            ]
        ]
        [ code_ [] ns ]
  , _fmtMath = \mt ns ->
      span_ 
        [ onCreatedWith_ (ActionRenderMath mt) ]
        ns
  }

mkLink :: action -> [View model action] -> View model action
mkLink action =
  a_ 
    [ onClick action
    , CSS.style_ 
      [ CSS.textDecoration "underline blue"
      , CSS.color CSS.blue
      , CSS.cursor "pointer" 
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

codeStyle :: CSS
codeStyle = Sheet $ CSS.sheet_
  [ CSS.selector_ "pre"
    [ CSS.border "1px solid black"
    , CSS.padding "10px"
    , CSS.backgroundColor #EEEEEE
    ]
  , CSS.selector_ "code.inlinecode"
    [ CSS.backgroundColor #EEEEEE
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

docTitle :: MisoString
docTitle = "MisoDoc"

-------------------------------------------------------------------------------
-- component
-------------------------------------------------------------------------------

mkComponent :: App Model Action
mkComponent = 
  (component mkModel updateModel viewModel)
    { initialAction = Just (ActionAskSummary "summary.md")
    , styles = 
      [ Href "https://cdn.jsdelivr.net/npm/katex@0.16.22/dist/katex.min.css"
      -- , Href "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.9.0/build/styles/default.min.css"
      , Href "github.min.css"
      , blockquoteStyle
      , codeStyle
      , tableStyle
      ]
    , scripts = 
        [ Src "https://cdn.jsdelivr.net/npm/katex@0.16.22/dist/katex.min.js"
        -- , Src "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.9.0/build/highlight.min.js"
        , Src "highlight.min.js"
        ]
    }


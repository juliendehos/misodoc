{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Helpers 
  ( renderPage
  , renderRaw
  , renderSummary
  ) where

import CMark
import Miso
import Miso.CSS qualified as CSS
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P

import Action

-------------------------------------------------------------------------------
-- export
-------------------------------------------------------------------------------

renderRaw :: MisoString -> View m a
renderRaw = renderNodeRaw . commonmarkToNode [] . fromMisoString

renderPage :: MisoString -> View m Action
renderPage = renderNodePage . commonmarkToNode [] . fromMisoString

renderSummary :: MisoString -> View m Action
renderSummary = renderNodeSummary . commonmarkToNode [] . fromMisoString

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

renderNodeRaw :: Node -> View m a
renderNodeRaw n = div_ [] [ text (ms (show n)) ]

renderNodePage :: Node -> View m Action
renderNodePage = \case
  Node _ DOCUMENT ns -> div_ [] (fmap renderNodePage ns)
  Node _ THEMATIC_BREAK ns -> hr_ []
  Node _ PARAGRAPH ns -> p_ [] (fmap renderNodePage ns)
  Node _ BLOCK_QUOTE ns -> pre_ [] (fmap renderNodePage ns)
  Node _ (HTML_BLOCK txt) ns -> span_ [] [ "TODO" ]
  Node _ (CUSTOM_BLOCK onenter onexit) ns -> span_ [] (fmap renderNodePage ns)
  Node _ (CODE_BLOCK info txt) ns -> pre_ [] (fmap renderNodePage ns)
  Node _ (HEADING x) ns -> fmtH x [] (fmap renderNodePage ns)
  Node _ (LIST attrs) ns -> fmtListAttrs attrs [] (fmap renderNodePage ns)
  Node _ ITEM ns -> li_ [] (fmap renderNodePage ns)
  Node _ (TEXT x) ns -> span_ [] (text (ms x) : fmap renderNodePage ns)
  Node _ SOFTBREAK ns -> span_ [] [ "TODO" ]
  Node _ LINEBREAK ns -> span_ [] [ "TODO" ]
  Node _ (HTML_INLINE txt) ns -> span_ [] [ "TODO" ]
  Node _ (CUSTOM_INLINE onenter onexit) ns -> span_ [] [ "TODO" ]
  Node _ (CODE txt) ns -> span_ [] (fmap renderNodePage ns)   -- TODO language + highlightjs
  Node _ EMPH ns -> em_ [] (fmap renderNodePage ns)
  Node _ STRONG ns -> strong_ [] (fmap renderNodePage ns)
  Node _ (LINK u t) ns -> a_ [ href_ (ms u) ] (text (ms t) : fmap renderNodePage ns)
  Node _ (IMAGE u t) ns -> span_ [] (img_ [ src_ (ms u), alt_ (ms t) ] : fmap renderNodePage ns)

renderNodeSummary :: Node -> View m Action
renderNodeSummary = \case
  Node _ DOCUMENT ns -> div_ [] (fmap renderNodeSummary ns)
  Node _ (LIST attrs) ns -> fmtListAttrs attrs [] (fmap renderNodeSummary ns)
  Node _ PARAGRAPH ns -> span_ [] (fmap renderNodeSummary ns)
  Node _ ITEM ns -> li_ [] (fmap renderNodeSummary ns)
  Node _ (LINK u t) ns -> fmtInternalLink (ms u) (text (ms t) : fmap renderNodeSummary ns) 
  Node _ (TEXT x) ns -> span_ [] (text (ms x) : fmap renderNodeSummary ns)
  _ -> span_ [] []

fmtInternalLink :: MisoString -> [View model Action] -> View model Action
fmtInternalLink u =
  a_ 
    [ onClick (ActionAskMd (ms u))
    , CSS.style_ 
      [ CSS.textDecoration "underline blue"
      , CSS.color CSS.blue
      , CSS.cursor "pointer" 
      ]
    ]

fmtH :: Level -> [Attribute action] -> [View model action] -> View model action
fmtH = \case
  1 -> h1_
  2 -> h2_
  3 -> h3_
  4 -> h4_
  5 -> h5_
  _ -> h6_

fmtListAttrs :: ListAttributes -> [Attribute action] -> [View model action] -> View model action
fmtListAttrs attrs = case listType attrs of
  BULLET_LIST -> ul_
  ORDERED_LIST -> ol_


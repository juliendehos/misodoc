{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Helpers 
  ( renderNode
  , renderPage
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

renderNode :: MisoString -> Node
renderNode = commonmarkToNode [] . fromMisoString

renderRaw :: Node -> View m a
renderRaw n = div_ [] [ text (ms (show n)) ]

renderPage :: Node -> View m Action
renderPage = \case
  Node _ DOCUMENT ns -> div_ [] (fmap renderPage ns)
  Node _ THEMATIC_BREAK ns -> hr_ []
  Node _ PARAGRAPH ns -> p_ [] (fmap renderPage ns)
  Node _ BLOCK_QUOTE ns -> pre_ [] (fmap renderPage ns)
  Node _ (HTML_BLOCK txt) ns -> span_ [] [ "TODO" ]
  Node _ (CUSTOM_BLOCK onenter onexit) ns -> span_ [] (fmap renderPage ns)
  Node _ (CODE_BLOCK info txt) ns -> pre_ [] (fmap renderPage ns)
  Node _ (HEADING x) ns -> fmtH x [] (fmap renderPage ns)
  Node _ (LIST attrs) ns -> fmtListAttrs attrs [] (fmap renderPage ns)
  Node _ ITEM ns -> li_ [] (fmap renderPage ns)
  Node _ (TEXT x) ns -> span_ [] (text (ms x) : fmap renderPage ns)
  Node _ SOFTBREAK ns -> span_ [] [ "TODO" ]
  Node _ LINEBREAK ns -> span_ [] [ "TODO" ]
  Node _ (HTML_INLINE txt) ns -> span_ [] [ "TODO" ]
  Node _ (CUSTOM_INLINE onenter onexit) ns -> span_ [] [ "TODO" ]
  Node _ (CODE txt) ns -> span_ [] (fmap renderPage ns)   -- TODO language + highlightjs
  Node _ EMPH ns -> em_ [] (fmap renderPage ns)
  Node _ STRONG ns -> strong_ [] (fmap renderPage ns)
  Node _ (LINK u t) ns -> a_ [ href_ (ms u) ] (text (ms t) : fmap renderPage ns)
  Node _ (IMAGE u t) ns -> span_ [] (img_ [ src_ (ms u), alt_ (ms t) ] : fmap renderPage ns)

renderSummary :: Node -> View m Action
renderSummary = \case
  Node _ DOCUMENT ns -> div_ [] (fmap renderSummary ns)
  Node _ (LIST attrs) ns -> fmtListAttrs attrs [] (fmap renderSummary ns)
  Node _ PARAGRAPH ns -> span_ [] (fmap renderSummary ns)
  Node _ ITEM ns -> li_ [] (fmap renderSummary ns)
  Node _ (LINK u t) ns -> fmtInternalLink (ms u) (text (ms t) : fmap renderSummary ns) 
  Node _ (TEXT x) ns -> span_ [] (text (ms x) : fmap renderSummary ns)
  _ -> span_ [] []

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

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


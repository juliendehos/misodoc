{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Helpers 
  where

import CMark
import Miso
import Miso.Html.Element as H
-- import Miso.Html.Event as E
import Miso.Html.Property as P

-------------------------------------------------------------------------------
-- export
-------------------------------------------------------------------------------

renderRaw :: MisoString -> View m a
renderRaw = viewRaw . commonmarkToNode [] . fromMisoString

renderMd :: MisoString -> View m a
renderMd = viewNode . commonmarkToNode [] . fromMisoString

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

viewRaw :: Node -> View m a
viewRaw n = div_ [] [ text (ms (show n)) ]

-- https://github.com/haskell-miso/miso-from-html
-- https://haddocks.haskell-miso.org/

viewNode :: Node -> View m a
viewNode = \case
  Node _ DOCUMENT ns -> div_ [] (fmap viewNode ns)
  Node _ THEMATIC_BREAK ns -> span_ [] [ "TODO" ]
  Node _ PARAGRAPH ns -> p_ [] (fmap viewNode ns)
  Node _ BLOCK_QUOTE ns -> span_ [] [ "TODO" ]
  Node _ (HTML_BLOCK txt) ns -> span_ [] [ "TODO" ]
  Node _ (CUSTOM_BLOCK onenter onexit) ns -> span_ [] [ "TODO" ]
  Node _ (CODE_BLOCK info txt) ns -> span_ [] [ "TODO" ]
  Node _ (HEADING x) ns -> fmtH x [] (fmap viewNode ns)
  Node _ (LIST attrs) ns -> span_ [] [ "TODO" ]
  Node _ ITEM ns -> span_ [] [ "TODO" ]
  Node _ (TEXT x) ns -> span_ [] (text (ms x) : fmap viewNode ns)
  Node _ SOFTBREAK ns -> span_ [] [ "TODO" ]
  Node _ LINEBREAK ns -> span_ [] [ "TODO" ]
  Node _ (HTML_INLINE txt) ns -> span_ [] [ "TODO" ]
  Node _ (CUSTOM_INLINE onenter onexit) ns -> span_ [] [ "TODO" ]
  Node _ (CODE txt) ns -> span_ [] [ "TODO" ]
  Node _ EMPH ns -> span_ [] [ "TODO" ]
  Node _ STRONG ns -> span_ [] [ "TODO" ]
  Node _ (LINK u t) ns -> a_ [ href_ (ms u) ] (text (ms t) : fmap viewNode ns)
  Node _ (IMAGE u t) ns -> span_ [] (img_ [ src_ (ms u), alt_ (ms t) ] : fmap viewNode ns)

fmtH :: Level -> [Attribute action] -> [View model action] -> View model action
fmtH = \case
  1 -> h1_
  2 -> h2_
  3 -> h3_
  4 -> h4_
  5 -> h5_
  _ -> h6_


{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Helpers 
  ( FmtChapterLink
  , getPreviousNext
  , parseChapters
  , renderNode
  , renderPage
  , renderPretty
  , renderRaw
  , renderSummary
  ) where

import CMark
import Miso
import Miso.CSS qualified as CSS
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P

-------------------------------------------------------------------------------
-- export
-------------------------------------------------------------------------------

type FmtChapterLink m a = MisoString -> [View m a] -> View m a

getPreviousNext :: [MisoString] -> MisoString -> (Maybe MisoString, Maybe MisoString)
getPreviousNext chapters current = go' chapters
  where
    go' = \case
      (x0:x1:x2:xs) -> if x0 == current
        then (Nothing, Just x1)
        else if x1 == current
          then (Just x0, Just x2)
          else go' (x1:x2:xs)
      [x0,x1] -> if x1 == current
        then (Just x0, Nothing)
        else (Nothing, Nothing)
      _ -> (Nothing, Nothing)

renderNode :: MisoString -> Node
renderNode = commonmarkToNode [] . fromMisoString

renderRaw :: Node -> View m a
renderRaw n = div_ [] [ text (ms (show n)) ]

parseChapters :: Node -> [MisoString]
parseChapters = \case
  Node _ DOCUMENT ns -> concatMap parseChapters ns
  Node _ (LINK u _) ns -> ms u : concatMap parseChapters ns
  Node _ (LIST _) ns -> concatMap parseChapters ns
  Node _ PARAGRAPH ns -> concatMap parseChapters ns
  Node _ ITEM ns -> concatMap parseChapters ns
  _ -> []

renderPage :: FmtChapterLink m a -> [MisoString] -> Node -> View m a
renderPage fmt chapterLinks = go'
  where
    go' = \case
      Node _ DOCUMENT ns -> div_ [] (fmap go' ns)
      Node _ THEMATIC_BREAK ns -> hr_ []
      Node _ PARAGRAPH ns -> p_ [] (fmap go' ns)
      Node _ BLOCK_QUOTE ns -> pre_ [] (fmap go' ns)
      Node _ (HTML_BLOCK txt) ns -> span_ [] [ "TODO" ]
      Node _ (CUSTOM_BLOCK onenter onexit) ns -> span_ [] (fmap go' ns)
      Node _ (CODE_BLOCK info txt) ns -> pre_ [] (fmap go' ns)
      Node _ (HEADING x) ns -> fmtH x [] (fmap go' ns)
      Node _ (LIST attrs) ns -> fmtListAttrs attrs [] (fmap go' ns)
      Node _ ITEM ns -> li_ [] (fmap go' ns)
      Node _ (TEXT x) ns -> span_ [] (text (ms x) : fmap go' ns)
      Node _ SOFTBREAK ns -> span_ [] [ "TODO" ]
      Node _ LINEBREAK ns -> span_ [] [ "TODO" ]
      Node _ (HTML_INLINE txt) ns -> span_ [] [ "TODO" ]
      Node _ (CUSTOM_INLINE onenter onexit) ns -> span_ [] [ "TODO" ]
      Node _ (CODE txt) ns -> span_ [] (fmap go' ns)   -- TODO language + highlightjs
      Node _ EMPH ns -> em_ [] (fmap go' ns)
      Node _ STRONG ns -> strong_ [] (fmap go' ns)
      Node _ (IMAGE u t) ns -> span_ [] (img_ [ src_ (ms u), alt_ (ms t) ] : fmap go' ns)
      Node _ (LINK u t) ns -> 
        let u' = ms u
        in if u' `elem` chapterLinks
          then fmt u' (text (ms t) : fmap (renderSummary fmt) ns) 
          else a_ [ href_ u' ] (text (ms t) : fmap (renderSummary fmt) ns) 

renderSummary :: FmtChapterLink m a -> Node -> View m a
renderSummary fmt = go'
  where
    go' = \case
      Node _ DOCUMENT ns -> div_ [] (fmap go' ns)
      Node _ (LIST attrs) ns -> fmtListAttrs attrs [] (fmap go' ns)
      Node _ PARAGRAPH ns -> span_ [] (fmap go' ns)
      Node _ ITEM ns -> li_ [] (fmap go' ns)
      Node _ (LINK u t) ns -> fmt (ms u) (text (ms t) : fmap go' ns)
      Node _ (TEXT x) ns -> span_ [] (text (ms x) : fmap go' ns)
      _ -> span_ [] []

-------------------------------------------------------------------------------
-- pretty printer
-------------------------------------------------------------------------------

renderPretty :: Node -> View m a
renderPretty = \case
  Node _ DOCUMENT ns -> pretty "DOCUMENT" ns
  Node _ (HEADING x) ns -> pretty (ms $ "HEADING " <> show x) ns
  Node _ PARAGRAPH ns -> pretty "PARAGRAPH" ns
  Node _ (LINK u t) ns -> pretty (ms $ "LINK " <> show u <> " " <> show t) ns
  Node _ (IMAGE u t) ns -> pretty (ms $ "IMAGE " <> show u <> " " <> show t) ns
  Node _ THEMATIC_BREAK ns -> pretty "THEMATIC_BREAK" ns
  Node _ BLOCK_QUOTE ns -> pretty "BLOCK_QUOTE" ns
  Node _ (HTML_BLOCK txt) ns -> pretty (ms $ "HTML_BLOCK " <> txt) ns
  Node _ (CUSTOM_BLOCK _ _) ns -> pretty "CUSTOM_BLOCK" ns
  Node _ (CODE_BLOCK info txt) ns -> pretty ( ms $ "CODE_BLOCK " <> info <> " " <> txt) ns
  Node _ (LIST attrs) ns -> pretty (ms $ "LIST " <> show attrs) ns
  Node _ ITEM ns -> pretty "ITEM" ns
  Node _ (TEXT x) ns -> pretty (ms $ "TEXT " <> show x) ns
  Node _ SOFTBREAK ns -> pretty "SOFTBREAK" ns
  Node _ LINEBREAK ns -> pretty "LINEBREAK" ns
  Node _ (HTML_INLINE txt) ns -> pretty (ms $ "HTML_INLINE " <> txt) ns
  Node _ (CUSTOM_INLINE _ _) ns -> pretty "CUSTOM_INLINE" ns
  Node _ (CODE txt) ns -> pretty (ms $ "CODE " <> show txt) ns
  Node _ EMPH ns -> pretty "EMPH" ns
  Node _ STRONG ns -> pretty "STRONG" ns
  where
    pretty :: MisoString -> [Node] -> View m a
    pretty name ns = div_ [] [ text (name <> ":"), ul_ [] (map (\n -> li_ [] [renderPretty n]) ns) ]

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

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


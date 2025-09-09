{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Helpers 
  ( Formatters(..)
  , getChapters
  , getPreviousNext
  , Node
  , parseNodes
  -- , renderPage
  -- , renderPretty
  , renderRaw
  , renderSummary
  ) where

import Data.List (foldl')
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Miso
import Miso.Html.Element as H
import Miso.Html.Property as P
import Text.Megaparsec.Error (errorBundlePretty)
import Text.MMark -- (MMark(..), parse)
import Text.MMark.Internal.Type
import qualified Text.URI as URI

-- https://hackage.haskell.org/package/mmark-0.0.8.0/docs/Text-MMark-Extension.html#t:Block
-- https://hackage.haskell.org/package/mmark-0.0.8.0/docs/Text-MMark-Extension.html#t:Inline

-------------------------------------------------------------------------------
-- export
-------------------------------------------------------------------------------

type Node = Bni

data Formatters m a = Formatters
  { _fmtChapterLink :: MisoString -> [View m a] -> View m a
  , _fmtInlineCode :: MisoString -> View m a
  , _fmtBlockQuote :: [View m a] -> View m a
  , _fmtCodeBlock :: [View m a] -> View m a
  }

parseNodes :: MisoString -> MisoString -> Either MisoString [Node]
parseNodes fp str = 
  case parse (fromMisoString fp) (fromMisoString str) of
    Left bundle -> Left $ ms $ errorBundlePretty bundle
    Right ns -> Right $ mmarkBlocks ns

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

getChapters :: [Node] -> [MisoString]
getChapters = concatMap goNode
  where

    goNode :: Node -> [MisoString]
    goNode = \case
      UnorderedList ns -> concatMap (concatMap goNode) ns
      OrderedList _ ns -> concatMap (concatMap goNode) ns
      Naked ns -> concatMap goInline ns
      Paragraph ns -> concatMap goInline ns
      _ -> []

    goInline :: Inline -> [MisoString]
    goInline = \case
      Link _ uri _ -> [ ms $ URI.render uri ]
      _ -> []   -- TODO

renderRaw :: [Node] -> View m a
renderRaw ns = div_ [] $ map (\n -> div_ [] [ text (ms (show n)) ]) ns

renderSummary :: Formatters m a -> [Node] -> View m a
renderSummary Formatters{..} ns = 
  div_ [] $ concatMap goNode ns   -- TODO concatMap -> map ?
  where
    -- goNode :: Node -> [View m a]
    goNode = \case
      UnorderedList ns -> [ ul_ [] (concatMap (concatMap goNode) ns) ]
      OrderedList _ ns -> [ ol_ [] (concatMap (concatMap goNode) ns) ]
      Naked ns -> concatMap goInline ns
      Paragraph ns -> concatMap goInline ns
      _ -> []
      
    -- goInline :: Inline -> [View m a]
    goInline = \case
      Link _ uri _ -> 
        let u = ms $ URI.render uri
        in [ li_ [] [ _fmtChapterLink u [ text u ] ] ]
      _ -> []   -- TODO


{-

renderSummary fmt = go'
  where
    go' = \case
      Node _ DOCUMENT ns -> div_ [] (fmap go' ns)
      Node _ (LIST attrs) ns -> fmtListAttrs attrs [] (fmap go' ns)
      Node _ PARAGRAPH ns -> span_ [] (fmap go' ns)
      Node _ ITEM ns -> li_ [] (fmap go' ns)
      Node _ (LINK u t) ns -> fmtChapterLink fmt (ms u) (text (ms t) : fmap go' ns)
      Node _ (TEXT x) ns -> span_ [] (text (ms x) : fmap go' ns)
      _ -> span_ [] []



renderPretty :: [Node] -> View m a
renderPretty = \case
  Node _ DOCUMENT ns -> pretty "DOCUMENT" ns
 
-}

{-

renderPage :: Formatters m a -> [MisoString] -> Node -> View m a
renderPage fmt chapterLinks = go'
  where
    go' = \case
      Node _ DOCUMENT ns -> div_ [] (fmap go' ns)
      Node _ THEMATIC_BREAK ns -> hr_ []
      Node _ PARAGRAPH ns -> if isTable ns then fmtTable ns else div_ [] (fmap go' ns)
      Node _ BLOCK_QUOTE ns -> fmtBlockQuote fmt (fmap go' ns)
      Node _ (HTML_BLOCK txt) ns -> span_ [] [ "TODO HTML_BLOCK" ]
      Node _ (CUSTOM_BLOCK onenter onexit) ns -> span_ [] (fmap go' ns)
      Node _ (CODE_BLOCK _info txt) _ -> fmtCodeBlock fmt [ text (ms txt) ]  -- TODO highlightjs ?
      Node _ (HEADING x) ns -> fmtH x [] (fmap go' ns)
      Node _ (LIST attrs) ns -> fmtListAttrs attrs [] (fmap go' ns)
      Node _ ITEM ns -> li_ [] (fmap go' ns)
      Node _ (TEXT x) ns -> span_ [] (text (ms x) : fmap go' ns)
      Node _ SOFTBREAK ns -> span_ [] [ "TODO SOFTBREAK" ]
      Node _ LINEBREAK ns -> span_ [] [ "TODO LINEBREAK" ]
      Node _ (HTML_INLINE txt) ns -> span_ [] [ "TODO HTML_INLINE" ]
      Node _ (CUSTOM_INLINE onenter onexit) ns -> span_ [] [ "TODO CUSTOM_INLINE" ]
      Node _ (CODE txt) _ -> (fmtInlineCode fmt (ms txt))
      Node _ EMPH ns -> em_ [] (fmap go' ns)
      Node _ STRONG ns -> strong_ [] (fmap go' ns)
      Node _ (IMAGE u t) ns -> span_ [] (img_ [ src_ (ms u), alt_ (ms t) ] : fmap go' ns)
      Node _ (LINK u t) ns -> 
        let u' = ms u
        in if u' `elem` chapterLinks
          then fmtChapterLink fmt u' (text (ms t) : fmap (renderSummary fmt) ns) 
          else a_ [ href_ u' ] (text (ms t) : fmap (renderSummary fmt) ns) 

      Node _ STRIKETHROUGH _ -> div_ [] [ "TODO STRIKETHROUGH" ]
      Node _ (TABLE _) _ -> div_ [] [ "TODO TABLE" ]
      Node _ TABLE_ROW _ -> div_ [] [ "TODO ROW" ]
      Node _ TABLE_CELL _ -> div_ [] [ "TODO CELL" ]
      Node _ FOOTNOTE_REFERENCE _ -> div_ [] [ "TODO FOOTNOTE_REFERENCE" ]
      Node _ FOOTNOTE_DEFINITION _ -> div_ [] [ "TODO FOOTNOTE_DEFINITION" ]

    isTable = \case
      (Node _ (TEXT x) _ : _) -> "|" `T.isPrefixOf` x
      _ -> False

    fmtTable ns0 = 
      let ns1 = flip filter ns0 $ \case 
                    (Node _ (TEXT x) _) -> not ("|-" `T.isPrefixOf` x)
                    (Node _ SOFTBREAK _) -> False     -- TODO split on SOFTBREAK
                    _ -> True
      in p_ [] [ table_ [] ( map (\n -> tr_ [] [ td_ [] [go' n] ] ) ns1 ) ]

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
    pretty name ns = div_ [] [ text name, ul_ [] (map (\n -> li_ [] [renderPretty n]) ns) ]

-}

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------



{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Helpers 
  ( Formatters(..)
  , getChapters
  , getPreviousNext
  , Node
  , parseNodes
  , renderPage
  , renderRaw
  , renderSummary
  ) where

-- TODO import Data.List (foldl')
import Miso (MisoString, View, fromMisoString, ms, text)
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
      _ -> []

renderRaw :: [Node] -> View m a
renderRaw ns = div_ [] $ map (\n -> div_ [] [ text (ms (show n)) ]) ns

renderSummary :: Formatters m a -> [Node] -> View m a
renderSummary Formatters{..} ns0 = 
  div_ [] $ concatMap goNode ns0

  where
    goNode = \case
      UnorderedList ns -> [ ul_ [] (concatMap (concatMap goNode) ns) ]
      OrderedList _ ns -> [ ol_ [] (concatMap (concatMap goNode) ns) ]
      Naked ns -> concatMap goInline ns
      Paragraph ns -> concatMap goInline ns
      _ -> []
      
    goInline = \case
      Plain txt -> [ text (ms txt) ]
      Image _ uri _ -> [ img_ [ src_ (ms $ URI.render uri) ] ]
      Link _ uri _ -> 
        let u = ms $ URI.render uri
        in [ li_ [] [ _fmtChapterLink u [ text u ] ] ]
      _ -> []   -- TODO strong, emphasize...

renderPage :: Formatters m a -> [MisoString] -> [Node] -> View m a
renderPage _ _ _ =
-- renderPage Formatters{..} chapterLinks ns0 = 
  div_ [] []    -- TODO


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

-}



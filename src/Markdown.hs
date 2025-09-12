{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Markdown 
  ( Formatter(..)
  , getChapters
  , getPreviousNext
  , Node
  , parseNodes
  , renderNodes
  , renderRaw
  ) where

import Commonmark.Simple
import Miso (MisoString, View, fromMisoString, ms, text)
import Miso.Html.Element as H
import Miso.Html.Property as P
import Text.Pandoc.Definition

-------------------------------------------------------------------------------
-- references
-------------------------------------------------------------------------------

-- https://hackage-content.haskell.org/package/pandoc-types-1.23.1/docs/Text-Pandoc-Definition.html#t:Block
-- https://hackage-content.haskell.org/package/pandoc-types-1.23.1/docs/Text-Pandoc-Definition.html#t:Inline
-- https://hackage.haskell.org/package/commonmark-0.2.6.1/docs/src/Commonmark.Html.html#line-106
-- https://hackage.haskell.org/package/commonmark-0.2.6.1/docs/src/Commonmark.Html.html#line-78

-------------------------------------------------------------------------------
-- export
-------------------------------------------------------------------------------

type Node = Block

data Formatter m a = Formatter
  { _fmtChapterLink :: MisoString -> [View m a] -> View m a
  , _fmtCodeBlock :: MisoString -> [View m a] -> View m a
  , _fmtMath :: MathType -> [View m a] -> View m a
  }

parseNodes :: MisoString -> MisoString -> Either MisoString [Block]
parseNodes fp str =
  case parseMarkdown (fromMisoString fp) (fromMisoString str) of
    Left err -> Left $ ms err
    Right (Pandoc _ bs) -> Right bs

renderRaw :: [Block] -> View m a
renderRaw ns = div_ [] $ map (\n -> div_ [] [ text (ms (show n)) ]) ns

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

getChapters :: [Block] -> [MisoString]
getChapters = concatMap goBlock
  where

    goBlock = \case
      BulletList bss -> concatMap goBlock $ concat bss
      OrderedList _ bss -> concatMap goBlock $ concat bss
      Para is -> concatMap goInline is
      Plain is -> concatMap goInline is
      _ -> []

    goInline = \case
      Link _ _ (url, _) -> [ms url]
      _ -> []

mkItem :: Bool -> [View m a] -> [View m a]
mkItem inList vs = if inList then [ li_ [] vs ] else vs

renderNodes :: Formatter m a -> [MisoString] -> [Block] -> View m a
renderNodes Formatter{..} chapterLinks bs0 = 
  div_ [] $ concatMap (goBlock False) bs0
  where

    fmtRows fRow fCell = 
      concatMap (\(Row _ cells) -> 
        [ fRow (concatMap (\(Cell _ _ _ _ bs) -> 
          [ fCell (concatMap (goBlock False) bs) ]) cells) ])

    goBlock inList = \case
      Plain is -> mkItem inList (concatMap goInline is)
      Para is -> mkItem inList [ p_ [] $ concatMap goInline is ]
      CodeBlock (_, lang, _) txt ->
        let langClass = 
              case lang of
                []    -> "nohighlight"
                (l:_) -> "language-" <> ms l
        in [ _fmtCodeBlock langClass [ text (ms txt) ] ]
      -- TODO LineBlock
      -- TODO RawBlock
      BlockQuote bs -> [ blockquote_ [] $ concatMap (goBlock False) bs ]
      OrderedList _ bss -> [ ol_ [] (concatMap (concatMap (goBlock True)) bss) ]
      BulletList bss -> [ ul_ [] (concatMap (concatMap (goBlock True)) bss) ]
      -- TODO DefinitionList 
      Header l _ is -> [ fmtH l $ concatMap goInline is ]
      HorizontalRule -> [ hr_ [] ]
      Table _ _ _ (TableHead _ ths) tbs _ ->
        [ table_ [] 
          (  fmtRows (tr_ []) (th_ []) ths
          ++ concatMap (\(TableBody _ _ _ tds) -> 
                fmtRows (tr_ []) (td_ []) tds) tbs
          )
        ]
      -- TODO Figure
      -- TODO Div
      _ -> []

    goInline = \case
      Str txt -> [ text (ms txt) ]
      Emph is -> [ em_ [] (concatMap goInline is) ]
      Underline is -> [ u_ [] (concatMap goInline is) ]
      Strong is -> [ strong_ [] (concatMap goInline is) ]
      Superscript is -> [ sup_ [] (concatMap goInline is) ]
      Subscript is -> [ sub_ [] (concatMap goInline is) ]
      -- TODO SmallCaps
      -- TODO Quoted
      -- TODO Cite
      Code _ txt -> [ code_ [] [ text (ms txt) ] ]
      Space -> [ " " ]
      SoftBreak -> [ "\n" ]
      LineBreak -> [ br_ [] ]
      Math mathType txt -> [ _fmtMath mathType [ text (ms txt) ] ]
      -- TODO RawInline
      Link _ is (url, _) -> 
        let urlStr = ms url
            mkLink = 
              if urlStr `elem` chapterLinks 
              then _fmtChapterLink urlStr
              else a_ [ href_ urlStr ]
        in [ mkLink (concatMap goInline is) ]
      Image _ _ (url, _) -> [ img_ [ src_ (ms url) ] ]    -- TODO alt
      -- TODO Note
      Span _ is -> [ span_ [] (concatMap goInline is) ]
      _ -> []

fmtH :: Int -> [View m a] -> View m a
fmtH = \case
  1 -> h1_ []
  2 -> h2_ []
  3 -> h3_ []
  4 -> h4_ []
  5 -> h5_ []
  6 -> h6_ []
  _ -> p_ []


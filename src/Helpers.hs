{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Helpers 
  ( Formatters(..)
  , getChapters
  , getPreviousNext
  , Block
  , parseMD
  , renderPage
  , renderRaw
  , renderSummary
  ) where

import Commonmark.Simple
import Miso (MisoString, View, fromMisoString, ms, text)
import Miso.Html.Element as H
import Miso.Html.Property as P
import Text.Pandoc.Definition

-- https://hackage-content.haskell.org/package/pandoc-types-1.23.1/docs/Text-Pandoc-Definition.html#t:Block
-- https://hackage-content.haskell.org/package/pandoc-types-1.23.1/docs/Text-Pandoc-Definition.html#t:Inline
-- https://hackage.haskell.org/package/commonmark-0.2.6.1/docs/src/Commonmark.Html.html#InlineElement

-- TODO parser errors

-- TODO emoji
-- TODO math
-- TODO autolinks
-- TODO task list

-------------------------------------------------------------------------------
-- export
-------------------------------------------------------------------------------

data Formatters m a = Formatters
  { _fmtChapterLink :: MisoString -> [View m a] -> View m a
  , _fmtInlineCode :: MisoString -> View m a
  , _fmtBlockQuote :: [View m a] -> View m a
  , _fmtCodeBlock :: MisoString -> View m a
  }

parseMD :: MisoString -> MisoString -> Either MisoString [Block]
parseMD fp str =
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
      _ -> []

    goInline = \case
      Link _ _ (url, _) -> [ms url]
      _ -> []


mkItem :: Bool -> [View m a] -> [View m a]
mkItem isList vs = if isList then [ li_ [] vs ] else vs

renderSummary :: Formatters m a -> [Block] -> View m a
renderSummary Formatters{..} bs0 = 
  div_ [] $ concatMap (goBlock False) bs0
  where

    goBlock isList = \case
      BulletList bss -> [ ul_ [] (concatMap (concatMap (goBlock True)) bss) ]
      OrderedList _ bss -> [ ol_ [] (concatMap (concatMap (goBlock True)) bss) ]
      Para is -> mkItem isList $ concatMap goInline is
      _ -> []
      
    goInline = \case
      Str txt -> [ text (ms txt) ]
      Emph is -> [ em_ [] (concatMap goInline is) ]
      Strong is -> [ strong_ [] (concatMap goInline is) ]
      Code _ txt -> [ code_ [] [ text (ms txt) ] ]
      LineBreak -> [ br_ [] ]
      Link _ is (url, _) -> [ _fmtChapterLink (ms url) (concatMap goInline is) ]
      Image _ _ (url, _) -> [ img_ [ src_ (ms url) ] ]    -- TODO alt
      Space -> [ " " ]
      _ -> []


renderPage :: Formatters m a -> [MisoString] -> [Block] -> View m a
renderPage Formatters{..} chapterLinks bs0 = 
  div_ [] $ concatMap (goBlock False) bs0
  where

    goBlock isList = \case
      BulletList bss -> [ ul_ [] (concatMap (concatMap (goBlock True)) bss) ]
      OrderedList _ bss -> [ ol_ [] (concatMap (concatMap (goBlock True)) bss) ]
      Para is -> mkItem isList $ concatMap goInline is
      Header l _ is -> [ fmtH l $ concatMap goInline is ]
      HorizontalRule -> [ hr_ [] ]
      BlockQuote bs -> [ _fmtBlockQuote $ concatMap (goBlock False) bs ]
      CodeBlock _ txt -> [ _fmtCodeBlock (ms txt) ]
      -- TODO Table
      _ -> []
      
    goInline = \case
      Str txt -> [ text (ms txt) ]
      Emph is -> [ em_ [] (concatMap goInline is) ]
      Strong is -> [ strong_ [] (concatMap goInline is) ]
      Code _ txt -> [ _fmtInlineCode (ms txt) ]
      LineBreak -> [ br_ [] ]
      Link _ is (url, _) -> 
        let urlStr = ms url
            mkLink = 
              if urlStr `elem` chapterLinks 
              then _fmtChapterLink urlStr
              else a_ [ href_ urlStr ]
        in [ mkLink (concatMap goInline is) ]
      Image _ _ (url, _) -> [ img_ [ src_ (ms url) ] ]    -- TODO alt
      Space -> [ " " ]
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


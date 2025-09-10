{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Helpers 
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

-- https://hackage-content.haskell.org/package/pandoc-types-1.23.1/docs/Text-Pandoc-Definition.html#t:Block
-- https://hackage-content.haskell.org/package/pandoc-types-1.23.1/docs/Text-Pandoc-Definition.html#t:Inline
-- https://hackage.haskell.org/package/commonmark-0.2.6.1/docs/src/Commonmark.Html.html#line-106
-- https://hackage.haskell.org/package/commonmark-0.2.6.1/docs/src/Commonmark.Html.html#line-78

-- TODO parse errors
-- TODO math
-- TODO table


-------------------------------------------------------------------------------
-- export
-------------------------------------------------------------------------------

type Node = Block

newtype Formatter m a = Formatter
  { _fmtChapterLink :: MisoString -> [View m a] -> View m a
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

    goBlock inList = \case
      Plain is -> concatMap goInline is
      BulletList bss -> [ ul_ [] (concatMap (concatMap (goBlock True)) bss) ]
      OrderedList _ bss -> [ ol_ [] (concatMap (concatMap (goBlock True)) bss) ]
      Para is -> mkItem inList [ div_ [] $ concatMap goInline is ]
      Header l _ is -> [ fmtH l $ concatMap goInline is ]
      HorizontalRule -> [ hr_ [] ]
      BlockQuote bs -> [ blockquote_ [] $ concatMap (goBlock False) bs ]
      CodeBlock _ txt ->    -- TODO language
        [ pre_ [ class_ "codeblock" ] [ code_ [] [ text (ms txt) ] ] ]
      -- TODO Table
      _ -> []
      
    goInline = \case
      Str txt -> [ text (ms txt) ]
      Emph is -> [ em_ [] (concatMap goInline is) ]
      Strong is -> [ strong_ [] (concatMap goInline is) ]
      Code _ txt -> [ code_ [] [ text (ms txt) ] ]
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


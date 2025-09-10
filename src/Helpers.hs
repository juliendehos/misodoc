{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Helpers 
  ( Formatters(..)
  , getChapters
  , getPreviousNext
  , Node
  , parseNodes
  , renderNodes
  , renderRaw
  ) where

import Miso (MisoString, View, fromMisoString, ms, text)
import Miso.Html.Element as H
import Miso.Html.Property as P
import Text.Megaparsec.Error (errorBundlePretty)
import Text.MMark
import Text.MMark.Internal.Type
import qualified Text.URI as URI

-- https://hackage.haskell.org/package/mmark-0.0.8.0/docs/Text-MMark-Extension.html#t:Block
-- https://hackage.haskell.org/package/mmark-0.0.8.0/docs/Text-MMark-Extension.html#t:Inline
-- https://hackage.haskell.org/package/mmark-0.0.8.0/docs/src/Text.MMark.Render.html#defaultBlockRender
-- https://hackage.haskell.org/package/mmark-0.0.8.0/docs/src/Text.MMark.Render.html#defaultInlineRender

-- TODO math
-- TODO code skylighting
-- TODO autolink
-- TODO emoji
-- TODO task list

-------------------------------------------------------------------------------
-- export
-------------------------------------------------------------------------------

type Node = Bni

data Formatters m a = Formatters
  { _fmtChapterLink :: MisoString -> [View m a] -> View m a
  , _fmtBlockQuote :: [View m a] -> View m a
  , _fmtCodeBlock :: [View m a] -> View m a
  }

parseNodes :: MisoString -> MisoString -> Either MisoString [Node]
parseNodes fp str = 
  case parse (fromMisoString fp) (fromMisoString str) of
    Left bundle -> Left $ ms $ errorBundlePretty bundle
    Right bs -> Right $ mmarkBlocks bs

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
getChapters = concatMap goBlock
  where

    goBlock :: Node -> [MisoString]
    goBlock = \case
      UnorderedList bs -> concatMap (concatMap goBlock) bs
      OrderedList _ bs -> concatMap (concatMap goBlock) bs
      Naked bs -> concatMap goInline bs
      Paragraph bs -> concatMap goInline bs
      _ -> []

    goInline :: Inline -> [MisoString]
    goInline = \case
      Link _ uri _ -> [ ms $ URI.render uri ]
      _ -> []

renderRaw :: [Node] -> View m a
renderRaw bs = div_ [] $ map (\n -> div_ [] [ text (ms (show n)) ]) bs

mkItem :: Bool -> [View m a] -> [View m a]
mkItem inList vs = if inList then [ li_ [] vs ] else vs

renderNodes :: Formatters m a -> [MisoString] -> [Node] -> View m a
renderNodes Formatters{..} chapterLinks bs0 = 
  div_ [] $ concatMap (goBlock False) bs0
  where

    goBlock inList = \case
      UnorderedList bs -> [ ul_ [] (concatMap (concatMap (goBlock True)) bs) ]
      OrderedList _ bs -> [ ol_ [] (concatMap (concatMap (goBlock True)) bs) ]
      Paragraph bs -> mkItem inList $ concatMap goInline bs
      Naked bs -> concatMap goInline bs
      ThematicBreak -> [ hr_ [] ]
      Heading1 bs -> [ h1_ [] (concatMap goInline bs) ]
      Heading2 bs -> [ h2_ [] (concatMap goInline bs) ]
      Heading3 bs -> [ h3_ [] (concatMap goInline bs) ]
      Heading4 bs -> [ h4_ [] (concatMap goInline bs) ]
      Heading5 bs -> [ h5_ [] (concatMap goInline bs) ]
      Heading6 bs -> [ h6_ [] (concatMap goInline bs) ]
      -- CodeBlock
      -- Blockquote
      -- Table
      _ -> []

    goInline = \case
      Plain txt -> [ text (ms txt) ]
      LineBreak -> [ br_ [] ]
      Emphasis is -> [ em_ [] (concatMap goInline is) ]
      Strong is -> [ strong_ [] (concatMap goInline is) ]
      Strikeout is -> [ del_ [] (concatMap goInline is) ]
      Subscript is -> [ sub_ [] (concatMap goInline is) ]
      Superscript is -> [ sup_ [] (concatMap goInline is) ]
      CodeSpan txt -> [ code_ [] [ text (ms txt) ] ]
      Image _ uri _ -> [ img_ [ src_ (ms $ URI.render uri) ] ]
      Link is uri _ -> 
        let u = ms $ URI.render uri
            t = concatMap goInline is
        in if u `elem` chapterLinks
          then [ _fmtChapterLink u t ]
          else [ a_ [ href_ u ] t ]


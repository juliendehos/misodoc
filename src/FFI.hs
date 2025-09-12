{-# LANGUAGE OverloadedStrings #-}

module FFI where

import Control.Monad (void)
import Language.Javascript.JSaddle

renderCode :: JSVal -> JSM ()
renderCode domref =
  void $ jsg ("hljs"::JSString) # ("highlightElement"::JSString) $ domref

renderMath :: JSVal -> JSM ()
renderMath domref = do
  eq <- domref ! ("innerHTML" :: JSString)
  void $ jsg ("katex"::JSString) # ("render"::JSString) $ [ eq, domref ]


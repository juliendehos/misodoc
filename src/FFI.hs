
module FFI where

import Control.Monad (void)
import Language.Javascript.JSaddle
import Text.Pandoc.Definition (MathType(..))

renderCode :: JSVal -> JSM ()
renderCode domref =
  void $ jsg "hljs" # "highlightElement" $ domref

renderMath :: MathType -> JSVal -> JSM ()
renderMath mathtype domref = do
  eq <- domref ! "innerHTML"
  opts <- create
  opts <# "throwOnError" $ False
  opts <# "displayMode" $ (mathtype == DisplayMath)
  optsVal <- toJSVal opts
  void $ jsg "katex" # "render" $ [ eq, domref, optsVal ]


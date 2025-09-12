
module FFI where

import Control.Monad (void)
import Language.Javascript.JSaddle

renderCode :: JSVal -> JSM ()
renderCode domref =
  void $ jsg "hljs" # "highlightElement" $ domref

renderMath :: JSVal -> JSM ()
renderMath domref = do
  eq <- domref ! "innerHTML"
  void $ jsg "katex" # "render" $ [ eq, domref ]


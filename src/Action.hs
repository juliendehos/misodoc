
module Action 
  ( Action(..)
  , MathType(..)
  ) where

import Miso
import Text.Pandoc.Definition

data Action
  = ActionFetchError MisoString (Response MisoString)
  | ActionAskPage MisoString
  | ActionSetPage MisoString (Response MisoString)
  | ActionAskSummary MisoString
  | ActionSetSummary MisoString (Response MisoString)
  | ActionSwitchDebug
  | ActionRenderCode DOMRef
  | ActionRenderMath MathType DOMRef


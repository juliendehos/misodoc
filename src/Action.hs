
module Action where

import Miso

data Action
  = ActionFetchError MisoString (Response MisoString)
  | ActionAskPage MisoString
  | ActionSetPage MisoString (Response MisoString)
  | ActionAskSummary MisoString
  | ActionSetSummary MisoString (Response MisoString)
  | ActionSwitchDebug
  | ActionRenderCode DOMRef
  | ActionRenderMath DOMRef



module Action where

import Miso

data Action
  = ActionError MisoString
  | ActionAskMd MisoString
  | ActionSetMd MisoString
  | ActionAskSummary MisoString
  | ActionSetSummary MisoString
  | ActionSwitchDebug


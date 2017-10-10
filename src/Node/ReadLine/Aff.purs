-- | This module provides an interface in Aff to Node.ReadLine
module Node.ReadLine.Aff
  ( setPrompt
  , close
  , question
  , readLine
  , module RLExports
  ) where

import Prelude (Unit, discard, pure, (<<<), (*>))
import Data.Either(Either(..))
import Node.ReadLine 
  ( output, completer, terminal, historySize, noCompletion,  createInterface,createConsoleInterface
  , Completer, Interface
  ) as RLExports
import Node.ReadLine as RL
import Control.Monad.Aff

-- | Writes a query to the output, waits
-- | for user input to be provided on input, then invokes
-- | the callback function
question
  :: forall eff
   . RL.Interface
  -> String
  -> Aff (readline :: RL.READLINE | eff) String
question interface q = makeAff go
  where
    go handler = RL.question q (handler <<< Right) interface *> pure nonCanceler

-- | Set the prompt.
setPrompt
  :: forall eff
   . String
  -> Int
  -> RL.Interface
  -> Aff (readline :: RL.READLINE | eff) Unit
setPrompt prompt len interface = liftEff' (RL.setPrompt prompt len interface)

-- | Close the specified `RL.Interface`.
close
  :: forall eff
   . RL.Interface
  -> Aff (readline :: RL.READLINE | eff) Unit
close = liftEff' <<< RL.close

readLine 
  :: forall eff
  . RL.Interface
  -> Aff (readline :: RL.READLINE | eff) String
readLine interface = makeAff go
  where
    go handler = do
      RL.setLineHandler interface (handler <<< Right) 
      RL.prompt interface
      pure nonCanceler
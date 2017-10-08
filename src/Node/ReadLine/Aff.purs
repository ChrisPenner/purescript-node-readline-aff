-- | This module provides an interface in Aff to Node.ReadLine
module Node.ReadLine.Aff
  ( createInterface
  , createConsoleInterface
  , prompt
  , setPrompt
  , setLineHandler
  , close
  , question
  , module RLExports
  ) where

import Prelude
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception
import Data.Options
import Node.Stream (Readable)
import Node.ReadLine (output, completer, terminal, historySize, noCompletion, Completer, Interface) as RLExports
import Node.ReadLine as RL
import Control.Monad.Aff

-- | Builds an interface with the specified options.
createInterface
  :: forall r eff
   .  Readable r (readline :: RL.READLINE | eff)
  -> Options RL.InterfaceOptions
  -> Aff (readline :: RL.READLINE | eff) RL.Interface
createInterface = RL.createInterface

-- | Create an interface with the specified completion function.
createConsoleInterface
  :: forall eff
   . RL.Completer (readline :: RL.READLINE, console :: CONSOLE, exception :: EXCEPTION | eff)
  -> Aff (readline :: RL.READLINE, console :: CONSOLE, exception :: EXCEPTION | eff) RL.Interface
createConsoleInterface = RL.createConsoleInterface

-- | Prompt the user for input on the specified `RL.Interface`.
prompt
  :: forall eff
   . RL.Interface
  -> Aff (readline :: RL.READLINE | eff) Unit
prompt = RL.prompt

-- | Writes a query to the output, waits
-- | for user input to be provided on input, then invokes
-- | the callback function
question
  :: forall eff
   . String
  -> (String -> Aff (readline :: RL.READLINE | eff) Unit)
  -> RL.Interface
  -> Aff (readline :: RL.READLINE | eff) Unit
question = RL.question

-- | Set the prompt.
setPrompt
  :: forall eff
   . String
  -> Int
  -> RL.Interface
  -> Aff (readline :: RL.READLINE | eff) Unit
setPrompt = RL.setPrompt

-- | Close the specified `RL.Interface`.
close
  :: forall eff
   . RL.Interface
  -> Aff (readline :: RL.READLINE | eff) Unit
close = RL.close

-- | Set the current line handler function.
setLineHandler
  :: forall eff a
   . RL.Interface
  -> RL.LineHandler (readline :: RL.READLINE | eff) a
  -> Aff (readline :: RL.READLINE | eff) Unit
setLineHandler = RL.setLineHandler

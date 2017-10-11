-- | This module provides an interface in Aff to Node.ReadLine
-- | 
-- | Example usage:
-- |
-- | ```
-- | import Node.ReadLine (close) as RL
-- | import Node.ReadLine.Aff (question, setPrompt, prompt, READLINE, createConsoleInterface, noCompletion)
-- | main :: forall e. Eff (console :: CONSOLE, readline :: READLINE, exception :: EXCEPTION | e) Unit
-- | main = do
-- |   interface <- createConsoleInterface noCompletion 
-- |   runAff_ (either 
-- |             (\err -> showError err *> RL.close interface) 
-- |             (const $ RL.close interface)) 
-- |           (loop interface)
-- |   where
-- |     showError err = error (show err) 
-- |     loop interface = do
-- |       setPrompt "$ " interface
-- |       dog <- question "What's your dog's name?\n" interface
-- |       liftEff <<< log $ ("Can I pet " <> dog <> "?")
-- |       str <- prompt interface
-- |       case uncons str of
-- |         Just {head: 'y'} -> liftEff $ log "Thanks!"
-- |         _ -> (liftEff $ log "C'mon! Be a sport about it!") *> loop interface
-- | ````
module Node.ReadLine.Aff
  ( close
  , prompt
  , question
  , setPrompt
  , module RLExports
  ) where

import Control.Monad.Aff

import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Either (Either(..))
import Data.String (length)
import Node.ReadLine (output, completer, terminal, historySize, noCompletion, createInterface, createConsoleInterface, Completer, Interface, InterfaceOptions, READLINE) as RLExports
import Node.ReadLine as RL
import Prelude (Unit, discard, pure, ($), ($>), (<<<))

-- | Writes a query to the output and returns the response
question
  :: forall eff m
   . MonadAff (readline :: RL.READLINE | eff) m
  => String
  -> RL.Interface
  -> m String
question q interface = do
  liftAff $ makeAff go
  where
    go handler = RL.question q (handler <<< Right) interface $> nonCanceler

-- | Set the prompt, this is displayed for future `prompt` calls.
setPrompt
  :: forall eff m
  . MonadEff (readline :: RL.READLINE | eff) m
  => String
  -> RL.Interface
  -> m Unit
setPrompt promptText interface =
  liftEff $ RL.setPrompt promptText (length promptText) interface

-- | Read a single line from input using the current prompt.
prompt 
  :: forall eff m
  . MonadAff (readline :: RL.READLINE | eff) m 
  => RL.Interface
  -> m String
prompt interface = do
  liftAff $ makeAff go
  where
    go handler = do
      RL.setLineHandler interface (handler <<< Right) 
      RL.prompt interface
      pure nonCanceler

-- | Close the specified Interface. This should upon error, or when you're done reading input.
close
  :: forall eff m
  . MonadEff (readline :: RL.READLINE | eff) m 
  => RL.Interface
  -> m Unit
close interface = liftEff (RL.close interface)

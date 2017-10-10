-- | This module provides an interface in Aff to Node.ReadLine
module Node.ReadLine.Aff
  ( setPrompt
  , close
  , question
  , readLine
  , module RLExports
  ) where

import Control.Monad.Aff

import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Either (Either(..))
import Node.ReadLine (output, completer, terminal, historySize, noCompletion, createInterface, createConsoleInterface, Completer, Interface) as RLExports
import Node.ReadLine as RL
import Prelude (class Monad, Unit, bind, discard, pure, ($), (*>), (<<<), (>>=), (>>>))

-- | Writes a query to the output, waits
-- | for user input to be provided on input, then invokes
-- | the callback function
question
  :: forall eff m
   . Monad m
  => MonadAff (readline :: RL.READLINE | eff) m
  => MonadAsk RL.Interface m
  => String
  -> m String
question q = do
  interface <- ask
  liftAff $ makeAff (go interface)
  where
    go interface handler = RL.question q (handler <<< Right) interface *> pure nonCanceler

-- | Set the prompt.
setPrompt
  :: forall eff m
  . MonadEff (readline :: RL.READLINE | eff) m
  => MonadAsk RL.Interface m
  => String
  -> Int
  -> m Unit
setPrompt prompt len = do
  interface <- ask 
  liftEff $ RL.setPrompt prompt len interface

-- | Close the specified `RL.Interface`.
close
  :: forall eff m
  . MonadEff (readline :: RL.READLINE | eff) m 
  => MonadAsk RL.Interface m
  => m Unit
close = ask >>= (RL.close >>> liftEff)

readLine 
  :: forall eff m
  . MonadAff (readline :: RL.READLINE | eff) m 
  => MonadAsk RL.Interface m
  => m String
readLine = do
  interface <- ask
  liftAff $ makeAff (go interface)
  where
    go interface handler = do
      RL.setLineHandler interface (handler <<< Right) 
      RL.prompt interface
      pure nonCanceler
-- | This module provides an interface in Aff to Node.ReadLine
-- | 
-- | Most actions provided must be run inside a monad conforming to `MonadAsk Interface`;
-- | this allows you to provide an `Interface` only once when you run the monadic block
-- | and it will be used by default by all combinators in the block. You can create an `Interface`
-- | using either `createInterface` or `createConsoleInterface` which are re-exported from `Node.ReadLine`.
-- |
-- | To run `MonadAff (readline :: RL.READLINE | eff) m => MonadAsk RL.Interface m` down to the
-- | Eff Monad you need to use an interface with runReaderT and provide a result handler to `runAff`
-- | as usual. It might look something like this: 
-- |
-- | ```
-- | runAff_ (either (error <<< show) log) (runReaderT loop interface)
-- | ```
-- | 
-- | Example usage:
-- |
-- | ```
-- | main :: forall e. Eff (console :: CONSOLE, readline :: READLINE, exception :: EXCEPTION | e) Unit
-- | main = do
-- |   interface <- createConsoleInterface noCompletion 
-- |   runAff_ (either (error <<< show) log) (runReaderT loop interface)
-- |   where
-- |     loop = do
-- |       setPrompt "$ "
-- |       dog <- question "What's your dog's name?\n"
-- |       liftEff <<< log $ "Can I pet " <> dog <> "?"
-- |       str <- readLine
-- |       case uncons str of
-- |         Just {head: 'y'} -> liftEff $ log "Thanks!"
-- |         _ -> liftEff $ log "C'mon! Be a sport about it!"
-- |       loop
-- | ````
module Node.ReadLine.Aff
  ( setPrompt
  , question
  , readLine
  , module RLExports
  ) where

import Control.Monad.Aff

import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Either (Either(..))
import Data.String (length)
import Node.ReadLine (output, completer, terminal, historySize, noCompletion, createInterface, createConsoleInterface, Completer, Interface) as RLExports
import Node.ReadLine as RL
import Prelude (Unit, bind, discard, pure, ($), ($>), (<<<), (>>=), (>>>))

-- | Writes a query to the output and returns the response
question
  :: forall eff m
   . MonadAff (readline :: RL.READLINE | eff) m
  => MonadAsk RL.Interface m
  => String
  -> m String
question q = do
  interface <- ask
  liftAff $ makeAff (go interface)
  where
    go interface handler = RL.question q (handler <<< Right) interface $> nonCanceler

-- | Set the prompt, this is displayed for future `readLine` calls.
setPrompt
  :: forall eff m
  . MonadEff (readline :: RL.READLINE | eff) m
  => MonadAsk RL.Interface m
  => String
  -> m Unit
setPrompt prompt = do
  interface <- ask 
  liftEff $ RL.setPrompt prompt (length prompt) interface

-- | Close the specified Interface.
close
  :: forall eff m
  . MonadEff (readline :: RL.READLINE | eff) m 
  => MonadAsk RL.Interface m
  => m Unit
close = ask >>= (RL.close >>> liftEff)

-- | Read a single line from input using the current prompt.
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
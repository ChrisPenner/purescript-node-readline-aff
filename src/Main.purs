module Main where

import Control.Monad.Aff (runAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, error, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.String (uncons)
import Prelude (Unit, bind, const, discard, show, ($), (*>), (<<<), (<>))
import Node.ReadLine (close) as RL
import Node.ReadLine.Aff (question, setPrompt, prompt, READLINE, createConsoleInterface, noCompletion)

main :: forall e. Eff (console :: CONSOLE, readline :: READLINE, exception :: EXCEPTION | e) Unit
main = do
  interface <- createConsoleInterface noCompletion 
  runAff_ (either 
            (\err -> showError err *> RL.close interface) 
            (const $ RL.close interface)) 
          (loop interface)
  where
    showError err = error (show err) 
    loop interface = do
      setPrompt "$ " interface
      dog <- question "What's your dog's name?\n" interface
      liftEff <<< log $ ("Can I pet " <> dog <> "?")
      str <- prompt interface
      case uncons str of
        Just {head: 'y'} -> liftEff $ log "Thanks!"
        _ -> (liftEff $ log "C'mon! Be a sport about it!") *> loop interface
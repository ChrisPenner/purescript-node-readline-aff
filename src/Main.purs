module Main where

import Control.Monad.Aff (runAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (Maybe(..))
import Data.String (uncons)
import Node.ReadLine (READLINE, createConsoleInterface, noCompletion)
import Node.ReadLine.Aff (question, readLine)
import Prelude (Unit, bind, const, discard, pure, unit, ($), (<<<), (<>))

main :: forall e. Eff (console :: CONSOLE, readline :: READLINE, exception :: EXCEPTION | e) Unit
main = do
  interface <- createConsoleInterface noCompletion 
  runAff_ (const $ pure unit) (runReaderT loop interface)
  where
    loop = do
      dog <- question "What's your dog's name?\n"
      liftEff <<< log $ "Can I pet " <> dog <> "?"
      str <- readLine
      case uncons str of
        Just {head: 'y'} -> liftEff $ log "Thanks!"
        _ -> liftEff $ log "C'mon! Be a sport about it!"
      loop
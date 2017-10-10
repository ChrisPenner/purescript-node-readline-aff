module Main where

import Control.Monad.Aff (liftEff', runAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.ReadLine (createConsoleInterface, noCompletion, READLINE)
import Node.ReadLine.Aff (question, readLine)
import Prelude (Unit, bind, const, discard, pure, unit, ($), (<<<), (<>))

main :: forall e. Eff (console :: CONSOLE, readline :: READLINE, exception :: EXCEPTION | e) Unit
main = do
  interface <- createConsoleInterface noCompletion 
  runAff_ (const $ pure unit) (loop interface)
  where
    loop interface = do
      dog <- question interface "What's your dog?\n"
      liftEff' <<< log $ "Can I pet " <> dog <> "?"
      str <- readLine interface
      liftEff' $ log str
      s2 <- readLine interface
      liftEff' <<< log $ "second:" <> s2
      loop interface
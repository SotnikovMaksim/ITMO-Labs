module Main (main) where

import Data.Set
import HW5.Evaluator (eval)
import HW5.Parser (parse)
import HW5.Pretty (prettyValue)
import Control.Monad.IO.Class (MonadIO(..))
import HW5.Action (HiPermission(..), HIO(..))
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      userInput <- getInputLine "hi> "
      case userInput of
        Nothing  -> return ()
        Just ""  -> loop
        Just "q" -> return ()
        Just input -> do
          case parse input of
            Left parseError -> outputStrLn $ show parseError
            Right parsed -> do
             res <- liftIO $ runHIO (eval parsed) (fromList [AllowRead, AllowWrite, AllowTime])
             case res of
               Left evalError -> outputStrLn $ show evalError
               Right val -> outputStrLn $ show $ prettyValue val
          loop

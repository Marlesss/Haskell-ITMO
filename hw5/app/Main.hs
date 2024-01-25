{-# LANGUAGE TypeApplications #-}

module Main (main) where
import System.Console.Haskeline
import HW5.Parser
import HW5.Evaluator (eval)
import HW5.Action
import HW5.Pretty
import qualified Data.Set as Set
import qualified Data.Text as T
import Control.Monad.IO.Class
import Control.Exception

main :: IO ()
main = runInputT defaultSettings $ repl executeHi

repl :: (String -> InputT IO ()) -> InputT IO ()
repl execute = loop where
  loop :: InputT IO ()
  loop = do
    minput <- getInputLine "hi> "
    case strip <$> minput of
      Nothing -> return ()
      Just input | input == ":q" || input == ":quit" -> outputStrLn "Leaving Hi" >> return ()
                 | input == ":?" || input == ":help" -> printHelp >> loop
                 | input == ""                       -> loop
                 | otherwise                         -> execute input >> loop

strip :: String -> String
strip = T.unpack . T.strip . T.pack

printHelp :: InputT IO ()
printHelp = do
  outputStrLn ":? or :help Prints this message"
  outputStrLn ":q Leaves Hi"
  outputStrLn "<Hi expression> Evaluates expression of Hi language"

executeHi :: String -> InputT IO ()
executeHi input = case parse input of
  (Left parseError) -> liftIO $ putDocLn $ prettyParseErrorBundle parseError
  (Right hiExpr)    -> do
    evalRes <- liftIO $ (try @PermissionException) (runHIO (eval hiExpr) (Set.fromList [AllowRead, AllowWrite]))
    case evalRes of
      (Left permException)    -> liftIO $ putDocLn $ prettyPermissionException permException
      (Right (Left hiError))  -> liftIO $ putDocLn $ prettyHiError hiError
      (Right (Right hiValue)) -> liftIO $ putDocLn $ prettyValue hiValue

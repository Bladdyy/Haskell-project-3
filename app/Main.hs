module Main where
import qualified Data.Map as Map
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Reducer
import Common
import Parser

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["--help"] -> 
          putStrLn "Please pass following argument: file - path to the file with program to reduce."
      [f] -> do
              fileExists <- doesFileExist f
              if fileExists then run f
              else error "ERROR: Invalid argument. File path does not exist."
      _   -> error "ERROR: Invalid arguments. Check out --help for more information."
    where
        run :: String -> IO ()
        run path = 
          do
           out <- fromHsString path
           let mapping = matchToMap out
           case Map.lookup start_state mapping of
             Nothing -> error "ERROR: No comibnator named main."
             _ -> performSteps (StateDesc (Var start_state, Top) 
                                  (snoc mempty (Var start_state, Top)) steps False mapping)
                    





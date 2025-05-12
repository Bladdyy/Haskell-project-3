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
        run path = do
                 out <- fromHsString path
                 let mapping = matchToMap out
                 print mapping
                 case Map.lookup "main" mapping of
                   Nothing -> error "ERROR: No comibnator named main."
                   Just (Def [Match _ _ expr]) ->
                      let x = fromList [(Var "main", Top)]
                          y = snoc x (expr, Top)
                          v = snoc y (left (expr, Top))
                          g = snoc v (left(right (expr, Top)))
                          z = snoc g (right(right (expr, Top)))

                      in
                      print (fmap showLoc z)
                          -- do
                          --   mapM_ print out
                          --   putStrLn "------------------------------------------------------------"
                          --   print expr
                          --   iterateSteps steps expr mapping
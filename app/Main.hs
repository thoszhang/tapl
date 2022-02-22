module Main where

import           Arith                    (eval, parse, printTerm)
import           System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
    where
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine "> "
            case minput of
                Nothing -> return ()
                Just input -> do
                    outputStrLn $ maybe "error" (printTerm . eval) (parse input)
                    loop

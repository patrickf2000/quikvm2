module Common(checkArgs) where

import System.Exit

-- Check command line arguments
checkArgs args = do
    if (length args) == 0
        then do
            putStrLn "Error: Input not specified!"
            exitFailure
        else return ()

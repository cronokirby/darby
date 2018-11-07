module Darby
    ( runDarby
    )
where

import Relude

import Darby.Context (ContextM, runCLIContextM)



runDarby :: IO ()
runDarby = runCLIContextM darby

-- | Entry point into the main program
darby :: ContextM ()
darby = putTextLn "Running Darby..."

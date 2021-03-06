{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Description: Contains a Context, and a CL arg parser for it

Also exports a ContextM monad for having a computation
around this context.
-}
module Darby.Context
   ( Context(..)
   , argsParser
   , ContextM(..)
   , runCLIContextM
   )
where

import Relude

import Options.Applicative


{- | Contains the Context for the Darby program.

This is currently restricted to just the directory
to shuffle, but will be expanded later.
-}
data Context = Context
    { contextDirectory :: FilePath
    , contextShuffle :: Bool
    , contextJustDisplay :: Bool
    }


contextParser :: Parser Context
contextParser = Context
    <$> strArgument (metavar "DIRECTORY")
    <*> flag True False
        (  long "noshuffle"
        <> help "Set this to not shuffle the songs"
        )
    <*> switch
        (  long "display"
        <> help "Print out the list without playing it"
        )
    
version :: Parser (a -> a)
version = infoOption "Darby version 0.1.0" 
    (  long "version" 
    <> short 'v'
    <> help "Display what version the program is using"
    )

argsParser :: ParserInfo Context
argsParser = info (contextParser <**> helper <**> version)
    (  fullDesc
    <> progDesc "Shuffle and play the songs in FILE"
    )


-- | Represents a computation with access to a Context
newtype ContextM a = ContextM
    { getContextM :: ReaderT Context IO a
    }
    deriving ( Functor, Applicative, Monad
             , MonadReader Context, MonadIO
             )

-- | Parses command line arguments to run a ContextM
runCLIContextM :: ContextM a -> IO a
runCLIContextM (ContextM m) =
    execParser argsParser >>= runReaderT m

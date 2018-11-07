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
newtype Context = Context
    { contextDirectory :: FilePath
    }


contextParser :: Parser Context
contextParser = Context
    <$> strArgument (metavar "DIRECTORY")

argsParser :: ParserInfo Context
argsParser = info (contextParser <**> helper)
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

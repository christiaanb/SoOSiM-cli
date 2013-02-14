{-# LANGUAGE DeriveDataTypeable #-}
module SoOSiM.CLI.Args where

import System.Console.CmdArgs

data SoOSiMCLI
  = SoOSiMCLI
  { example   :: FilePath
  , batchMode :: Bool
  } deriving (Show,Data,Typeable)

cliArgs = SoOSiMCLI { example   = def &= typFile &= args
                    , batchMode = def
                    }

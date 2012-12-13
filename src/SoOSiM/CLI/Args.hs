{-# LANGUAGE DeriveDataTypeable #-}
module SoOSiM.CLI.Args where

import System.Console.CmdArgs

data SoOSiMCLI
  = SoOSiMCLI
  { example :: FilePath
  } deriving (Data,Typeable)

cliArgs = SoOSiMCLI { example = def &= argPos 0 &= typFile }

{-# LANGUAGE FlexibleContexts #-}
module SoOSiM.CLI where

import qualified Data.Foldable            as F
import           Data.IntMap              (IntMap)
import           Control.Applicative      ((<$>))
import           Control.Monad.IfElse     (whenM)
import           Control.Monad.Writer     (MonadWriter,WriterT,execWriterT,listen,tell,lift,liftIO)
import           System.Console.CmdArgs   (cmdArgs)
import           System.Console.Haskeline
import qualified System.Directory         as Directory

import SoOSiM
import SoOSiM.Types
import SoOSiM.Examples.Loader

import SoOSiM.CLI.Args
import SoOSiM.CLI.Util

main :: IO ()
main = do
  f <- fmap example $ cmdArgs cliArgs
  whenM (Directory.doesFileExist f) $ do
    s <- loader f
    k <- execWriterT $ runInputT defaultSettings (loop s)
    putStrLn $ unlines k
    return ()

loop :: SimState -> InputT (WriterT [String] IO) ()
loop s = do
  let clk = simClk s
      ns  = nodes s
  ((ns',comps),msgs) <- lift $ listen $ printNodes ns
  outputStrLn (unlines comps)
  outputStrLn (unlines msgs)
  minput <- getInputLine "% "
  case minput of
    Nothing   -> (liftIO $ tick (s {nodes = ns'})) >>= loop
    Just ":q" -> return ()
    Just _    -> (liftIO $ tick (s {nodes = ns'})) >>= loop

printNodes ::
  Functor m
  => MonadWriter [String] m
  => IntMap Node
  -> m (IntMap Node, [String])
printNodes = mapAccumRM printNode []

printNode ::
  Functor m
  => MonadWriter [String] m
  => [String]
  -> Node
  -> m (Node, [String])
printNode r n = do
  (nC,s) <- mapAccumLM printComponentContext "" (nodeComponents n)
  let s' = "Node " ++ show (nodeId n) ++ ": [" ++ s ++ "]"
  return (n {nodeComponents = nC}, s':r)

printComponentContext ::
  MonadWriter [String] m
  => String
  -> ComponentContext
  -> m (ComponentContext, String)
printComponentContext s cc@(CC iface cid _ _ _ _ tBuf _) = do
  tell tBuf
  let cc' = cc {traceMsgs = []}
  let s'  = s ++ (componentName iface ++ "(" ++ show cid ++ ")") ++ "; "
  return $! (cc',s')

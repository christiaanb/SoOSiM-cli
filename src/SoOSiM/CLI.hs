{-# LANGUAGE FlexibleContexts #-}
module SoOSiM.CLI where

import qualified Data.Foldable            as F
import           Data.IntMap              (IntMap)
import           Control.Applicative      ((<$>))
import           Control.Concurrent.STM   (TVar,readTVarIO)
import           Data.List                (intersperse)
import           Control.Monad            (when,unless)
import           Control.Monad.IfElse     (whenM)
import           Control.Monad.Writer     (MonadWriter,MonadIO,WriterT,execWriterT,listen,tell,lift,liftIO)
import           System.Console.CmdArgs   (cmdArgs)
import           System.Console.Haskeline (InputT,defaultSettings,getInputLine,outputStrLn,runInputT)
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
    k <- execWriterT $ runInputT defaultSettings (outputStrLn helpText >> loopInteract s)
    putStrLn $ unlines k
    return ()

loopInteract :: SimState -> InputT (WriterT [String] IO) ()
loopInteract s = do
    let clk = simClk s
        ns  = nodes s
    ((ns',comps),msgs) <- lift $ listen $ printNodes ns
    let s' = s {nodes = ns'}
    outputStrLn "= System State ="
    outputStrLn (unlines comps)
    unless (null msgs) $ do outputStrLn "= Trace Messages ="
                            outputStrLn (unlines $ msgs)
    minput <- getInputLine "SoOSiM $ "
    when (running s) $ do
      case minput of
        Just "quit"   -> return ()
        Just "help"   -> outputStrLn helpText >> loopInteract s'
        Just "finish" -> loop Nothing s' >>= const (return ())
        Just ""       -> (liftIO $ tick s') >>= loopInteract
        _             -> outputStrLn helpText >> loopInteract s'
  where
    loop (Just n) s = do
      s' <- liftIO (tick s)
      ((ns',_),_)  <- lift $ listen $ printNodes (nodes s')
      let s'' = s' {nodes = ns'}
      if running s'
        then loop (Just (n-1)) s''
        else return s'
    loop (Just 0) s = return s
    loop Nothing  s = do
      s' <- liftIO (tick s)
      ((ns',_),_)  <- lift $ listen $ printNodes (nodes s')
      let s'' = s' {nodes = ns'}
      if running s'
        then loop Nothing s''
        else return s'

helpText :: String
helpText = unlines $
      [ "= Commands ="
      , "type 'quit' to quit"
      , "hit return to progress 1 step"
      , "type 'finish' to progress until a component calls 'stopSim'"
      ]

printNodes ::
  Functor m
  => MonadWriter [String] m
  => MonadIO m
  => IntMap Node
  -> m (IntMap Node, [String])
printNodes = mapAccumRM printNode []

printNode ::
  Functor m
  => MonadWriter [String] m
  => MonadIO m
  => [String]
  -> Node
  -> m (Node, [String])
printNode r n = do
  (nC,s) <- mapAccumLM printComponentContext [] (nodeComponents n)
  let s' = "Node " ++ show (nodeId n) ++ ":\n[ " ++ (concat $ intersperse "\n; " s) ++ "\n]"
  return (n {nodeComponents = nC}, s':r)

printComponentContext ::
  MonadWriter [String] m
  => MonadIO m
  => [String]
  -> ComponentContext
  -> m (ComponentContext, [String])
printComponentContext s cc@(CC iface cid _ stT _ msgBT tBuf _) = do
  tell tBuf
  let cc' = cc {traceMsgs = []}
  status  <- liftIO $ printStatus stT msgBT
  let s'  = (show cid ++ " :: " ++ componentName iface ++ " (" ++ status ++ ")"):s
  return $! (cc',s')

printStatus ::
  TVar (ComponentStatus s)
  -> TVar [a]
  -> IO String
printStatus stT msgBT = do
  st    <- readTVarIO stT
  msgBT <- readTVarIO msgBT
  case (st,msgBT) of
    (ReadyToIdle,[])     -> return "Idle"
    (ReadyToIdle,_)      -> return "Running"
    (WaitingFor cId _,_) -> return $! "Waiting for " ++ show cId
    (Running _ _,_)      -> return "Running"
    (ReadyToRun, _)      -> return "Running"
    (Killed,_)           -> return "Idle"

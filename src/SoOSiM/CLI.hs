{-# LANGUAGE FlexibleContexts #-}
module SoOSiM.CLI where

import qualified Data.Foldable            as F
import           Data.IntMap              (IntMap)
import           Data.Maybe               (catMaybes)
import           Data.Monoid              (Any(..))
import           Control.Applicative      ((<$>))
import           Control.Concurrent.STM   (TVar,readTVarIO)
import           Data.List                (intersperse)
import           Control.Monad            (when,unless)
import           Control.Monad.IfElse     (whenM)
import           Control.Monad.Writer     (MonadWriter,MonadIO,WriterT(..),execWriterT,listen,tell,lift,liftIO)
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
  options <- cmdArgs cliArgs
  let f = example options
  let b = batchMode options
  whenM (Directory.doesFileExist f) $ do
    s <- loader f
    k <- execWriterT $ runInputT defaultSettings
          (if b then loop Nothing s >> return () else outputStrLn helpText >> loopInteract s)
    putStrLn $ unlines $ catMaybes $ map snd k
    return ()

loopInteract :: SimState -> InputT (WriterT [(String,Maybe String)] IO) ()
loopInteract s = do
    let clk = simClk s
        ns  = nodes s
    (((ns',comps),anyRunning),msgs) <- lift $ listen $ runWriterT (printNodes ns)
    let s' = s {nodes = ns'}
    outputStrLn "= System State ="
    outputStrLn (unlines comps)
    unless (null msgs) $ do outputStrLn "= Trace Messages ="
                            outputStrLn (unlines $ map fst msgs)
    minput <- getInputLine "SoOSiM $ "
    when (running s && getAny anyRunning) $ do
      case minput of
        Just "quit"   -> return ()
        Just "help"   -> outputStrLn helpText >> loopInteract s'
        Just "finish" -> loop Nothing s' >>= const (return ())
        Just ""       -> (liftIO $ tick s') >>= loopInteract
        _             -> outputStrLn helpText >> loopInteract s'

loop (Just 0) s = return s
loop (Just n) s = do
  s' <- liftIO (tick s)
  (((ns',_),anyRunning),_)  <- lift $ listen $ runWriterT (printNodes (nodes s'))
  let s'' = s' {nodes = ns'}
  if (running s' && getAny anyRunning)
    then loop (Just (n-1)) s''
    else return s'
loop Nothing  s = do
  s' <- liftIO (tick s)
  (((ns',_),anyRunning),_)  <- lift $ listen $ runWriterT (printNodes (nodes s'))
  let s'' = s' {nodes = ns'}
  if (running s' && getAny anyRunning)
    then loop Nothing s''
    else return s'

helpText :: String
helpText = unlines $
      [ "= Commands ="
      , "type 'quit' to quit"
      , "hit return to progress 1 step"
      , "type 'finish' to progress until a component calls 'stopSim', or all are idle"
      ]

type PrintMonad a = WriterT Any (WriterT [(String,Maybe String)] IO) a

printNodes ::
  IntMap Node
  -> PrintMonad (IntMap Node, [String])
printNodes = mapAccumRM printNode []

printNode ::
  [String]
  -> Node
  -> PrintMonad (Node, [String])
printNode r n = do
  (nC,s) <- mapAccumLM printComponentContext [] (nodeComponents n)
  let s' = "Node " ++ show (nodeId n) ++ ":\n[ " ++ (concat $ intersperse "\n; " s) ++ "\n]"
  return (n {nodeComponents = nC}, s':r)

printComponentContext ::
  [String]
  -> ComponentContext
  -> PrintMonad (ComponentContext, [String])
printComponentContext s cc@(CC iface cid _ stT _ msgBT tBuf _) = do
  lift $ tell tBuf
  let cc' = cc {traceMsgs = []}
  status  <- printStatus stT msgBT
  let s'  = (show cid ++ " :: " ++ componentName iface ++ " (" ++ status ++ ")"):s
  return $! (cc',s')

printStatus ::
  TVar (ComponentStatus s)
  -> TVar [a]
  -> PrintMonad String
printStatus stT msgBT = do
  st    <- liftIO $ readTVarIO stT
  msgBT <- liftIO $ readTVarIO msgBT
  case (st,msgBT) of
    (ReadyToIdle,[])     -> return "Idle"
    (ReadyToIdle,_)      -> tell (Any True) >> return "Running"
    (WaitingFor cId _,_) -> tell (Any True) >> (return $! "Waiting for " ++ show cId)
    (Running _ _,_)      -> tell (Any True) >> return "Running"
    (ReadyToRun, _)      -> tell (Any True) >> return "Running"
    (Killed,_)           -> return "Idle"

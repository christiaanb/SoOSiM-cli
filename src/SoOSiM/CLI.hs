{-# LANGUAGE FlexibleContexts #-}
module SoOSiM.CLI where

import qualified Data.Foldable            as F
import           Data.IntMap              (IntMap)
import           Control.Applicative      ((<$>))
import           Control.Monad.IfElse     (whenM)
import           Control.Monad.Writer     (MonadWriter,WriterT,execWriterT,tell,lift,liftIO)
import           System.Console.CmdArgs   (cmdArgs)
import           System.Console.Haskeline
import qualified System.Directory         as Directory

import SoOSiM
import SoOSiM.Types
import SoOSiM.Examples.Loader

import SoOSiM.CLI.Args

main :: IO ()
main = do
  f <- fmap example $ cmdArgs cliArgs
  whenM (Directory.doesFileExist f) $ do
    s <- loader f
    k <- execWriterT $ runInputT defaultSettings (loop s)
    return ()

loop :: SimState -> InputT (WriterT [String] IO) ()
loop s = do
  let clk = simClk s
      ns  = nodes s
  k <- lift $ printNodes ns
  outputStrLn (unlines k)
  minput <- getInputLine "% "
  case minput of
    Nothing   -> (liftIO $ tick s) >>= loop
    Just ":q" -> return ()
    Just _    -> (liftIO $ tick s) >>= loop

printNodes :: Functor m => MonadWriter [String] m => IntMap Node -> m [String]
printNodes = F.foldrM
  (\n r -> (:r) <$> printNode n
  ) []

printNode :: Functor m => MonadWriter [String] m => Node -> m String
printNode n = (\x -> show (nodeId n) ++ ": " ++ x) <$> F.foldlM
  (\r c -> (\x -> r ++ x ++ "; ") <$> printComponentContext c
  ) "" (nodeComponents n)

printComponentContext :: MonadWriter [String] m => ComponentContext -> m String
printComponentContext (CC iface cid _ _ _ _ tBuf _) = do
  tell tBuf
  return $! (componentName iface ++ "(" ++ show cid ++ ")")

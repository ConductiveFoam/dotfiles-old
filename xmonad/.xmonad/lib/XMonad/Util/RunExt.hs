module XMonad.Util.RunExt
  ( inDirectory
  , inHomeDirectory, inHomeSubDirectory
  ) where

import XMonad (MonadIO, io, catchIO)
import System.Directory (setCurrentDirectory, getCurrentDirectory, getHomeDirectory)

inHomeDirectory :: MonadIO m => m a -> m a
inHomeDirectory = inHomeSubDirectory ""

inHomeSubDirectory, inDirectory :: MonadIO m => String -> m a -> m a
inHomeSubDirectory dir a = do
      home <- io $ getHomeDirectory
      inDirectory (home ++ dir) a

inDirectory dir a = do
      curr <- io getCurrentDirectory
      catchIO $ setCurrentDirectory dir
      result <- a
      catchIO $ setCurrentDirectory curr
      return result

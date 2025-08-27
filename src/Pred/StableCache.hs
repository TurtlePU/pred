module Pred.StableCache (StableCache, newStableCache, closeCache, request) where

import Data.Foldable (traverse_)
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import System.Mem.StableName (StableName)
import qualified System.Mem.StableName as StableName
import System.Mem.Weak (Weak)
import qualified System.Mem.Weak as Weak

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap

import Pred.Prelude

newtype StableCache k v = SC (IORef (HashMap (StableName k) (Weak v)))

newStableCache :: IO (StableCache k v)
newStableCache = SC <$> IORef.newIORef HashMap.empty

closeCache :: StableCache k v -> IO ()
closeCache (SC sc) = IORef.readIORef sc >>= traverse_ Weak.finalize

request :: MonadIO m => StableCache k v -> k -> IO (v, IO ()) -> m v
request (SC sc) key gen = liftIO do
  name <- StableName.makeStableName key
  IORef.readIORef sc
    >>= maybe (pure Nothing) Weak.deRefWeak . HashMap.lookup name
    >>= flip maybe pure do
      (v, finalize) <- gen
      w <- Weak.mkWeak key v $ Just do
        IORef.modifyIORef' sc (HashMap.delete name) >> finalize
      IORef.modifyIORef' sc $ HashMap.insert name w
      pure v

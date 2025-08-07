module Pred.Region (module Pred.Region, liftIO) where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Cont (ContT (..), evalContT)

type Region r a = ContT r IO a

(<...&>) :: IO a -> (a -> IO b) -> Region r a
create <...&> destroy = ContT (bracket create destroy)

region :: MonadIO m => Region a a -> m a
region = liftIO . evalContT

{-# LANGUAGE FunctionalDependencies #-}

module Blag.Routing (RoutingTable, Router (..), makeRoutingTable', makeRoutingTable, route, routeRev, routeNext, routePrev) where

import Blag.Prelude
import Control.Monad (forM, join)
import Data.Bimap qualified as BM
import Data.Function (fix)
import Data.Maybe (fromMaybe)

newtype RoutingTable = RoutingTable { unRoutingTable :: BM.Bimap FilePath FilePath }

infix 3 |->

class Router files router | router -> files where
  (|->) :: files -> router -> Rules [(FilePath, FilePath)]

instance Router [FilePattern] (FilePath -> Rules [FilePath]) where
  filePatterns |-> router = do
    files <- liftIO $ getDirectoryFilesIO "" filePatterns
    forEach files $ \src -> do
      stages <- router src
      return $ zip (src : stages) stages

instance Router [FilePattern] (FilePath -> Rules FilePath) where
  filePatterns |-> router = do
    files <- liftIO $ getDirectoryFilesIO "" filePatterns
    forEach files $ \src -> do
      out <- router src
      return $ [(src, out)]

instance Router FilePath FilePath where
  src |-> out = return [(src, out)]

makeRoutingTable' :: [(FilePath, FilePath)] -> Rules RoutingTable
makeRoutingTable' routes = do
  want (map snd routes)
  return $ makeRoutingTable routes

makeRoutingTable :: [(FilePath, FilePath)] -> RoutingTable
makeRoutingTable routes = RoutingTable (BM.fromList routes)

route :: (?routingTable :: RoutingTable, MonadFail m) => FilePath -> m FilePath
route src = case routeNext src of
  Nothing -> fail $ "No route from " <> src
  Just out -> return $ fix (\rec out -> maybe out rec $ routeNext out) out

routeRev :: (?routingTable :: RoutingTable, MonadFail m) => FilePath -> m FilePath
routeRev out = case routePrev out of
  Nothing -> fail $ "No route to " <> out
  Just src -> return $ fix (\rec src -> maybe src rec $ routePrev src) src

routeNext :: (?routingTable :: RoutingTable, MonadFail m) => FilePath -> m FilePath
routeNext src = case BM.lookup src (unRoutingTable ?routingTable) of
  Nothing -> fail $ "No route from " <> src
  Just out -> return out

routePrev :: (?routingTable :: RoutingTable, MonadFail m) => FilePath -> m FilePath
routePrev out = case BM.lookupR out (unRoutingTable ?routingTable) of
  Nothing -> fail $ "No route to " <> out
  Just src -> return src
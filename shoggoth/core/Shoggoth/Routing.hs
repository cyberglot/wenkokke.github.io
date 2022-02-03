module Shoggoth.Routing
  ( Anchor,
    RoutingTable,
    Router (..),
    cacheRoutingTable,
    route,
    routeSrc,
    routeNext,
    routePrev,
    routeAnchor,
    sources,
    outputs,
  )
where

import Control.Monad (forM, join, (>=>))
import Control.Monad.IO.Class (MonadIO)
import Data.Bimap qualified as Bimap
import Data.Function (fix)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Shoggoth.Prelude

type Anchor = Text

data RoutingTable = RoutingTable
  { routingTableSrcs :: Set.Set FilePath,
    routingTableOuts :: Set.Set FilePath,
    routingTableLnks :: Bimap.Bimap FilePath FilePath,
    routingTableAnks :: Map.Map (Anchor, FilePath) FilePath
  }

instance Semigroup RoutingTable where
  RoutingTable srcs1 outs1 lnks1 anks1 <> RoutingTable srcs2 outs2 lnks2 anks2 =
    RoutingTable (srcs1 <> srcs2) (outs1 <> outs2) (lnks1 <> lnks2) (anks1 <> anks2)

instance Monoid RoutingTable where
  mempty = RoutingTable mempty mempty mempty mempty

infix 3 |->

class Router router where
  (|->) :: [FilePattern] -> router -> Action RoutingTable

instance Router (FilePath -> Action [(Maybe Anchor, FilePath)]) where
  filePatterns |-> router = do
    srcs <- getDirectoryFiles "" filePatterns
    routingTables <- forM srcs $ \src -> do
      anksAndStgs <- router src
      let stgs = snd <$> anksAndStgs
      let out = last stgs
      let lnks = zip (src : stgs) stgs
      let anks = mapMaybe (\(maybeAnk, stg) -> fmap (\ank -> ((ank, src), stg)) maybeAnk) anksAndStgs
      return $ makeRoutingTable [src] [out] lnks anks
    return (mconcat routingTables)

instance Router (FilePath -> Action [FilePath]) where
  filePatterns |-> router = filePatterns |-> anchorlessRouter
    where
      anchorlessRouter :: FilePath -> Action [(Maybe Anchor, FilePath)]
      anchorlessRouter = fmap (fmap (Nothing,)) . router

instance Router (FilePath -> Action FilePath) where
  filePatterns |-> router = filePatterns |-> singleStageRouter
    where
      singleStageRouter :: FilePath -> Action [FilePath]
      singleStageRouter = fmap (: []) . router

instance Router (FilePath -> FilePath) where
  filePatterns |-> router = filePatterns |-> router
    where
      pureRouter :: FilePath -> Action FilePath
      pureRouter = return . router
instance Router FilePath where
  filePatterns |-> out = filePatterns |-> constRouter
    where
      constRouter :: FilePath -> FilePath
      constRouter = const out

makeRoutingTable :: [FilePath] -> [FilePath] -> [(FilePath, FilePath)] -> [((Anchor, FilePath), FilePath)] -> RoutingTable
makeRoutingTable srcs outs lnks anks =
  RoutingTable (Set.fromList srcs) (Set.fromList outs) (Bimap.fromList lnks) (Map.fromList anks)

cacheRoutingTable :: [Action RoutingTable] -> Rules (() -> Action RoutingTable)
cacheRoutingTable routingTables =
  newCache $ \() -> fmap mconcat (sequence routingTables)

-- | Route any path to its next stage, failing if there is no next stage.
routeNext :: (?getRoutingTable :: () -> Action RoutingTable) => FilePath -> Action FilePath
routeNext prev = do
  routingTable <- ?getRoutingTable ()
  maybe (fail $ "No route from " <> prev) return (maybeRouteNext routingTable prev)

-- | Route any path to its next stage. Return nothing if there is no next stage.
maybeRouteNext :: RoutingTable -> FilePath -> Maybe FilePath
maybeRouteNext RoutingTable {..} src = Bimap.lookup src routingTableLnks

-- | Route any path to its previous stage, failing if there is no previous stage.
routePrev :: (?getRoutingTable :: () -> Action RoutingTable) => FilePath -> Action FilePath
routePrev next = do
  routingTable <- ?getRoutingTable ()
  maybe (fail $ "No route to " <> next) return (maybeRoutePrev routingTable next)

-- | Route any path to its previous stage. Return nothing if there is no previous stage.
maybeRoutePrev :: RoutingTable -> FilePath -> Maybe FilePath
maybeRoutePrev RoutingTable {..} out = Bimap.lookupR out routingTableLnks

-- | Route any path to its final output.
route :: (?getRoutingTable :: () -> Action RoutingTable) => FilePath -> Action FilePath
route src = do
  routingTable <- ?getRoutingTable ()
  let iterRouteNext :: FilePath -> FilePath
      iterRouteNext = fix (\iter next -> maybe next iter $ maybeRouteNext routingTable next)
  iterRouteNext <$> routeNext src

-- | Route any path to its initial source.
routeSrc :: (?getRoutingTable :: () -> Action RoutingTable) => FilePath -> Action FilePath
routeSrc out = do
  routingTable <- ?getRoutingTable ()
  let iterRoutePrev :: FilePath -> FilePath
      iterRoutePrev = fix (\iter prev -> maybe prev iter $ maybeRoutePrev routingTable prev)
  iterRoutePrev <$> routePrev out

-- | Route any source path to a specific anchored stage.
routeAnchor :: (?getRoutingTable :: () -> Action RoutingTable) => Anchor -> FilePath -> Action FilePath
routeAnchor ank src = do
  RoutingTable {..} <- ?getRoutingTable ()
  maybe (fail $ "No anchor " <> show ank <> " for " <> src) return (Map.lookup (ank, src) routingTableAnks)

outputs :: RoutingTable -> [FilePath]
outputs routingTable = Set.toAscList $ routingTableOuts routingTable

sources :: RoutingTable -> [FilePath]
sources routingTable = Set.toAscList $ routingTableSrcs routingTable

instance (Ord a, Ord b) => Semigroup (Bimap.Bimap a b) where
  bm1 <> bm2 = foldr (uncurry Bimap.insert) bm1 (Bimap.assocs bm2)

instance (Ord a, Ord b) => Monoid (Bimap.Bimap a b) where
  mempty = Bimap.empty
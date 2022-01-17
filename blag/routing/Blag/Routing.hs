module Blag.Routing (Anchor, RoutingTable, Router (..), route, routeSrc, routeNext, routePrev, routeAnchor, sources, outputs) where

import Blag.Prelude
import Control.Monad (forM, join, (>=>))
import Data.Bimap qualified as Bimap
import Data.Function (fix)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set qualified as Set

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
  (|->) :: [FilePattern] -> router -> Rules RoutingTable

instance Router (FilePath -> Rules [(Maybe Anchor, FilePath)]) where
  filePatterns |-> router = do
    srcs <- liftIO $ getDirectoryFilesIO "" filePatterns
    routingTables <- forM srcs $ \src -> do
      anksAndStgs <- router src
      let stgs = snd <$> anksAndStgs
      let out = last stgs
      let lnks = zip (src : stgs) stgs
      let anks = mapMaybe (\(maybeAnk, stg) -> fmap (\ank -> ((ank, src), stg)) maybeAnk) anksAndStgs
      return $ makeRoutingTable [src] [out] lnks anks
    return (mconcat routingTables)

instance Router (FilePath -> Rules [FilePath]) where
  filePatterns |-> router = filePatterns |-> anchorlessRouter
    where
      anchorlessRouter :: FilePath -> Rules [(Maybe Anchor, FilePath)]
      anchorlessRouter = fmap (fmap (Nothing,)) . router

instance Router (FilePath -> Rules FilePath) where
  filePatterns |-> router = filePatterns |-> singleStageRouter
    where
      singleStageRouter :: FilePath -> Rules [FilePath]
      singleStageRouter = fmap (: []) . router

instance Router FilePath where
  filePatterns |-> out = filePatterns |-> constRouter
    where
      constRouter :: FilePath -> Rules FilePath
      constRouter _ = return out

makeRoutingTable :: [FilePath] -> [FilePath] -> [(FilePath, FilePath)] -> [((Anchor, FilePath), FilePath)] -> RoutingTable
makeRoutingTable srcs outs lnks anks =
  RoutingTable (Set.fromList srcs) (Set.fromList outs) (Bimap.fromList lnks) (Map.fromList anks)

route :: (MonadFail m, ?routingTable :: RoutingTable) => FilePath -> m FilePath
route src = case routeNext src of
  Nothing -> fail $ "No route from " <> src
  Just out -> return $ fix (\rec out -> maybe out rec $ routeNext out) out

routeSrc :: (MonadFail m, ?routingTable :: RoutingTable) => FilePath -> m FilePath
routeSrc out = case routePrev out of
  Nothing -> fail $ "No route to " <> out
  Just src -> return $ fix (\rec src -> maybe src rec $ routePrev src) src

routeNext :: (MonadFail m, ?routingTable :: RoutingTable) => FilePath -> m FilePath
routeNext src = case Bimap.lookup src (routingTableLnks ?routingTable) of
  Nothing -> fail $ "No route from " <> src
  Just out -> return out

routePrev :: (MonadFail m, ?routingTable :: RoutingTable) => FilePath -> m FilePath
routePrev out = case Bimap.lookupR out (routingTableLnks ?routingTable) of
  Nothing -> fail $ "No route to " <> out
  Just src -> return src

routeAnchor :: (MonadFail m, ?routingTable :: RoutingTable) => Anchor -> FilePath -> m FilePath
routeAnchor ank src = case Map.lookup (ank, src) (routingTableAnks ?routingTable) of
  Nothing -> fail $ "No anchor " <> show ank <> " for " <> src
  Just tmp -> return tmp

outputs :: RoutingTable -> [FilePath]
outputs routingTable = Set.toAscList $ routingTableOuts routingTable

sources :: RoutingTable -> [FilePath]
sources routingTable = Set.toAscList $ routingTableSrcs routingTable

instance (Ord a, Ord b) => Semigroup (Bimap.Bimap a b) where
  bm1 <> bm2 = foldr (uncurry Bimap.insert) bm1 (Bimap.assocs bm2)

instance (Ord a, Ord b) => Monoid (Bimap.Bimap a b) where
  mempty = Bimap.empty
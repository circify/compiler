module IR.SMT.MemRouteTest
  ( test_benesRoute
  )
where
import IR.SMT.Opt.Mem.Route
import qualified Data.List as DLs
import qualified Test.QuickCheck as QC

-- check that Benes is OK
test_benesRoute_ :: Int -> QC.Gen Bool
test_benesRoute_ len = do
    let ivals = take (4 + len `mod` 1024) [0..]
    ovals <- QC.shuffle ivals
    let (sw_i, sw_o) = benesRoute ivals ovals
        (it, ib) = benesTopBottom sw_i ivals
        (ot, ob) = benesTopBottom sw_o ovals
        imatch = it == DLs.sort ot
        omatch = ib == DLs.sort ob
    return $ imatch && omatch

test_benesRoute = QC.withMaxSuccess 16 test_benesRoute_

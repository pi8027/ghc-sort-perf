module ComparisonProvider (ComparisonTest(..)) where

import Test.Tasty.Providers (IsTest(..), testPassed)
import Data.Data (Typeable)
import Data.Tagged
import Data.IORef (newIORef, modifyIORef')
import GHC.IO (unsafePerformIO, evaluate)
import Control.DeepSeq (NFData(rnf))
import GHC.IORef (readIORef)

data ComparisonTest a = ComparisonTest
  { list :: ![a]
  , alg  :: !((a -> a -> Ordering) -> [a] -> [a])
  }

instance (Typeable a, Ord a, NFData a) => IsTest (ComparisonTest a) where
  run _ (ComparisonTest l sortBy) _ = do
    comps <- comparisons sortBy l
    pure $ testPassed (show comps ++ " comparisons")

  testOptions = Tagged []

comparisons :: (NFData t, Ord a) => ((a -> a -> Ordering) -> t -> t) -> t -> IO Int
comparisons sortBy xs = do
    v <- newIORef 0
    let cmp a b  = unsafePerformIO $ do
            modifyIORef' v succ
            pure $ a `compare` b
    evaluate $ rnf $ sortBy cmp xs
    readIORef v


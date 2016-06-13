module Main where

import Control.Concurrent
import Control.Exception      (PatternMatchFail (..), catch)
import Control.Monad.IO.Class
import Control.Monad.Timing
import Data.Tree
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "TimingT" $ do
        it "records timing" $ do
            (_, tree) <- runTimingT $
                timeGroup "foo" $ liftIO $ threadDelay 1000
            tree `shouldMatchPattern`
                (\ [ Node ("foo", x) [] ] -> x >= 0.001)

        it "condenses multiple groups into one" $ do
            (_, tree) <- runTimingT $ do
                timeGroup "foo" $ liftIO $ threadDelay 1000
                timeGroup "foo" $ liftIO $ threadDelay 2000
            tree `shouldMatchPattern`
                (\ [ Node ("foo", x) [] ] -> x >= 0.003)

        it "preserves subgroups" $ do
            (_, tree) <- runTimingT $ do
                timeGroup "foo" $ timeGroup "bar" $ liftIO $ threadDelay 1000
                timeGroup "foo" $ timeGroup "bar" $ liftIO $ threadDelay 2000
            tree `shouldMatchPattern`
                (\ [ Node ("foo", x) [ Node ("bar", y) [] ] ] -> x >= 0.003 && y >= 0.003)

shouldMatchPattern n f = catch
    (f n `shouldBe` True)
    (\ (PatternMatchFail s) -> expectationFailure $ "pattern did not match: " ++ show n)

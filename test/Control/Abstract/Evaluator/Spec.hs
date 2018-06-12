{-# LANGUAGE TypeOperators #-}
module Control.Abstract.Evaluator.Spec
( spec
) where

import Analysis.Abstract.Evaluating (evaluating)
import Control.Abstract
import Data.Abstract.Module
import qualified Data.Abstract.Number as Number
import Data.Abstract.Package
import Data.Abstract.Value as Value
import Data.Algebra
import Data.Bifunctor (first)
import Data.Functor.Const
import Data.Semilattice.Lower
import Data.Sum
import SpecHelpers hiding (reassociate)

spec :: Spec
spec = parallel $ do
  it "constructs integers" $ do
    (expected, _) <- evaluate (box (integer 123))
    expected `shouldBe` Right (Value.Integer (Number.Integer 123))

  it "calls functions" $ do
    (expected, _) <- evaluate $ do
      identity <- closure [name "x"] lowerBound (variable (name "x"))
      call identity [box (integer 123)]
    expected `shouldBe` Right (Value.Integer (Number.Integer 123))

evaluate
  = runM
  . evaluating @Precise @(Value Precise (Eff _))
  . runReader (PackageInfo (name "test") Nothing mempty)
  . runReader (ModuleInfo "test/Control/Abstract/Evaluator/Spec.hs")
  . fmap reassociate
  . runValueError
  . runEnvironmentError
  . runAddressError
  . runAllocator
  . (>>= deref . fst)
  . runEnv lowerBound
  . runReturn
  . runLoopControl

reassociate :: Either (SomeExc exc1) (Either (SomeExc exc2) (Either (SomeExc exc3) result)) -> Either (SomeExc (Sum '[exc3, exc2, exc1])) result
reassociate = mergeExcs . mergeExcs . mergeExcs . Right
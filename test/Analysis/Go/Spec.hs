{-# OPTIONS_GHC -O0 #-}
module Analysis.Go.Spec (spec) where

import qualified Data.Abstract.ModuleTable as ModuleTable
import qualified Data.Language as Language
import SpecHelpers


spec :: TaskSession -> Spec
spec session = parallel $ do
  describe "Go" $ do
    it "imports and wildcard imports" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main.go", "foo/foo.go", "bar/bar.go", "bar/rab.go"]
      case ModuleTable.lookup "main.go" <$> res of
        Right (Just (Module _ (scopeAndFrame, _))) -> do
          () <$ SpecHelpers.lookupDeclaration "foo" scopeAndFrame heap scopeGraph `shouldBe` Just ()
          SpecHelpers.lookupMembers "foo" Import scopeAndFrame heap scopeGraph `shouldBe` Just ["New"]
          () <$ SpecHelpers.lookupDeclaration "main" scopeAndFrame heap scopeGraph `shouldBe` Just ()
          () <$ SpecHelpers.lookupDeclaration "Bar" scopeAndFrame heap scopeGraph `shouldBe` Just ()
          () <$ SpecHelpers.lookupDeclaration "Rab" scopeAndFrame heap scopeGraph `shouldBe` Just ()
        other -> expectationFailure (show other)

    it "imports with aliases (and side effects only)" $ do
      (scopeGraph, (heap, res)) <- evaluate ["main1.go", "foo/foo.go", "bar/bar.go", "bar/rab.go"]
      case ModuleTable.lookup "main1.go" <$> res of
        Right (Just (Module _ (scopeAndFrame, _))) -> do
          const () <$> SpecHelpers.lookupDeclaration "f" scopeAndFrame heap scopeGraph `shouldBe` Just ()
          const () <$> SpecHelpers.lookupDeclaration "main" scopeAndFrame heap scopeGraph `shouldBe` Just ()
          -- (lookupDeclaration "f" heap >>= deNamespace heap) `shouldBe` Just ("f",  ["New"])
        other -> expectationFailure (show other)

  where
    fixtures = "test/fixtures/go/analysis/"
    evaluate = evalGoProject . map (fixtures <>)
    evalGoProject = testEvaluating <=< evaluateProject' session (Proxy :: Proxy 'Language.Go) goParser

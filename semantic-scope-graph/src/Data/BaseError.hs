{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Data.BaseError
  ( BaseError (..),
    throwBaseError,
  )
where

import Control.Effect.Reader
import Control.Effect.Resumable
import Control.Effect.State
import Control.Lens
import Data.Functor.Classes
import Data.Module (ModuleInfo)
import qualified Data.Module as Module
import Source.Loc (Loc)
import qualified Source.Loc as Loc
import Source.Span (Span)
import qualified Source.Span as Span
import qualified System.Path as Path

data BaseError (exc :: * -> *) resume = BaseError {baseErrorModuleInfo :: ModuleInfo, baseErrorSpan :: Maybe Span, baseErrorException :: exc resume}

instance (Show (exc resume)) => Show (BaseError exc resume) where
  showsPrec _ BaseError {..} = shows baseErrorException <> showString " " <> showString errorLocation
    where
      errorLocation
        | startErrorLine == endErrorLine = baseModuleFilePath <> " " <> startErrorLine <> ":" <> startErrorCol <> "-" <> endErrorCol
        | otherwise = baseModuleFilePath <> " " <> startErrorLine <> ":" <> startErrorCol <> "-" <> endErrorLine <> ":" <> endErrorCol
      baseModuleFilePath = Path.toString $ Module.modulePath baseErrorModuleInfo
      startErrorLine = show $ maybe 0 (Span.line . Span.start) baseErrorSpan
      endErrorLine = show $ maybe 0 (Span.line . Span.end) baseErrorSpan
      startErrorCol = show $ maybe 0 (Span.column . Span.start) baseErrorSpan
      endErrorCol = show $ maybe 0 (Span.column . Span.end) baseErrorSpan

instance (Eq1 exc) => Eq1 (BaseError exc) where
  liftEq f (BaseError info1 span1 exc1) (BaseError info2 span2 exc2) = info1 == info2 && span1 == span2 && liftEq f exc1 exc2

instance Show1 exc => Show1 (BaseError exc) where
  liftShowsPrec sl sp d (BaseError info span exc) = showParen (d > 10) $ showString "BaseError" . showChar ' ' . showsPrec 11 info . showChar ' ' . showsPrec 11 span . showChar ' ' . liftShowsPrec sl sp 11 exc

throwBaseError ::
  ( Has (Resumable (BaseError exc)) sig m,
    Has (Reader ModuleInfo) sig m,
    Has (Reader (Maybe Loc)) sig m
  ) =>
  exc resume ->
  m resume
throwBaseError err = do
  moduleInfo <- ask
  loc <- ask
  throwResumable $ BaseError moduleInfo (maybe Nothing (Just . Loc.span) loc) err

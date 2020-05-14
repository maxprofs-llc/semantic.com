{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | The ScopeGraph effect is used to build up a scope graph over
-- the lifetime of a monadic computation. The name is meant to evoke
-- physically sketching the hierarchical outline of a graph.
module Control.Effect.StackGraph
  ( ScopeGraph,
    StackGraphEff,
    ScopeError,
    declare,
    addDeclarations,
    -- Scope Manipulation
    currentScope,
    rootScope,
    putCurrentScope,
    -- newEdge,
    -- newReference,
    newScope,
    addBottomScope,
    addTopScope,
    connectScopes,
    withScope,
    declareFunction,
    declareMaybeName,
    declareParameter,
    -- reference,
    Has,
  )
where

import Analysis.Name (Name)
import qualified Analysis.Name as Name
import Control.Algebra
import Control.Effect.Fresh
import Control.Effect.Reader
import Control.Effect.Resumable
import qualified Control.Effect.StackGraph.Properties.Reference as Props
import qualified Control.Effect.StackGraph.Properties.Reference as Props.Reference
import Control.Effect.State
import Control.Lens
import Data.BaseError
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Module as Module
import qualified Data.ScopeGraph as ScopeGraph
import Data.Semilattice.Lower
import Data.Text (Text)
import GHC.Records
import Scope.Graph.AdjacencyList (ScopeGraph)
import qualified Scope.Graph.AdjacencyList as AdjacencyList
import qualified Scope.Reference as Reference
import Scope.Types
import Source.Loc
import Source.Span
import qualified Stack.Graph as Stack

-- | Extract the 'Just' of a 'Maybe' in an 'Applicative' context or, given 'Nothing', run the provided action.
maybeM :: Applicative f => f a -> Maybe a -> f a
maybeM f = maybe f pure
{-# INLINE maybeM #-}

type StackGraphEff sig m =
  ( Has (State (Stack.Graph Stack.Node)) sig m,
    Has (State (CurrentScope Name)) sig m,
    Has (Resumable (BaseError ScopeError)) sig m,
    Has (Reader (Maybe Loc)) sig m,
    Has (State (Maybe Loc)) sig m,
    Has (Reader Stack.Node) sig m, -- The root node of the module.
    Has (Reader Module.ModuleInfo) sig m,
    Has Fresh sig m
  )

currentScope :: StackGraphEff sig m => m (CurrentScope Name)
currentScope = get @(CurrentScope Name)

rootScope :: StackGraphEff sig m => m Stack.Node
rootScope = ask @Stack.Node

putCurrentScope :: StackGraphEff sig m => Name -> m ()
putCurrentScope = put . CurrentScope

withScope ::
  StackGraphEff sig m =>
  Name ->
  m a ->
  m a
withScope scope action = do
  CurrentScope s <- get @(CurrentScope Name)
  put (CurrentScope scope)
  x <- action
  put (CurrentScope s)
  pure x

declare :: StackGraphEff sig m => Name -> Kind -> Loc -> m Stack.Node
declare n kind loc = do
  let declNode = Stack.Declaration n kind loc
  -- ToDo: generate a unique id for the node in the graph
  pure declNode

refer :: StackGraphEff sig m => Name -> Kind -> Loc -> m Stack.Node
refer n kind loc = do
  let nameNode = Stack.Reference n kind loc
  -- ToDo: generate a unique id for the node in the graph
  return nameNode

addBottomScope :: StackGraphEff sig m => m Stack.Node
addBottomScope = do
  CurrentScope s <- get @(CurrentScope Name)
  modify (Stack.addEdge (Stack.BottomScope s) (Stack.Scope s))
  pure (Stack.BottomScope s)

addTopScope :: StackGraphEff sig m => m Stack.Node
addTopScope = do
  CurrentScope s <- get @(CurrentScope Name)
  modify (Stack.addEdge (Stack.TopScope s) (Stack.Scope s))
  pure (Stack.TopScope s)

connectScopes :: StackGraphEff sig m => Stack.Node -> Stack.Node -> m ()
connectScopes scopeA existingScope = do
  modify (Stack.addEdge scopeA existingScope)

newScope :: forall sig m. StackGraphEff sig m => Name -> m Name
newScope currentScope = do
  name <- Name.gensym
  name <$ modify (Stack.newScope name currentScope)

addDeclarations :: StackGraphEff sig m => NonEmpty (Name, Kind, Loc) -> m (Stack.Graph Stack.Node)
addDeclarations names = do
  let graph' =
        foldr
          ( \(name, kind, loc) graph ->
              Stack.addEdge (Stack.Declaration name kind loc) (Stack.PopSymbol ".") graph
          )
          mempty
          (NonEmpty.init names)
      graph'' = (Stack.addEdge (Stack.PopSymbol ".") ((\(name, kind, loc) -> (Stack.Declaration name kind loc)) (NonEmpty.last names)) graph')
      graph''' =
        foldr
          ( \(name, kind, loc) graph ->
              Stack.addEdge (Stack.Reference name kind loc) (Stack.PushSymbol ".") graph
          )
          mempty
          (NonEmpty.init $ NonEmpty.reverse names)
      graph'''' = Stack.overlay graph'' (Stack.addEdge (Stack.PushSymbol ".") ((\(name, kind, loc) -> (Stack.Reference name kind loc)) (NonEmpty.head names)) graph''')
      graph''''' = (\(name, kind, loc) -> Stack.addEdge (Stack.Declaration name kind loc) (Stack.Reference name kind loc) graph'''') (NonEmpty.last names)
  pure graph'''''

-- | Takes an edge label and a list of names and inserts an import edge to a hole.
-- newEdge :: StackGraphEff sig m => ScopeGraph.EdgeLabel -> NonEmpty Name -> m ()
-- newEdge label address = do
--   CurrentScope current <- currentScope
--   modify (ScopeGraph.addImportEdge label (toList address) current)
lookupScope :: StackGraphEff sig m => Stack.Symbol -> m Stack.Node
lookupScope symbol = do
  graph <- get @(Stack.Graph Stack.Node)
  maybeM (throwScopeError $ LookupScopeError) (Stack.lookupScope symbol graph)

throwScopeError ::
  ( Has (Resumable (BaseError ScopeError)) sig m,
    Has (Reader Module.ModuleInfo) sig m,
    Has (Reader (Maybe Loc)) sig m
  ) =>
  ScopeError resume ->
  m resume
throwScopeError = throwBaseError

data ScopeError return where
  LookupScopeError :: ScopeError Stack.Node

-- ScopeError :: Stack.Symbol -> Span -> ScopeError (Stack.Scope Symbol)
-- ImportReferenceError :: ScopeError (Stack.Scope Symbol)
-- LookupPathError :: Stack.Symbol -> ScopeError (Stack.Path)
-- LookupDeclarationScopeError :: Stack.Symbol -> ScopeError Name
-- DeclarationByNameError :: Stack.Symbol -> ScopeError (Info Stack.Symbol)
-- CurrentScopeError :: ScopeError Stack.Symbol

declareFunction :: forall sig m. StackGraphEff sig m => Maybe Name -> Kind -> Loc -> m (Stack.Node, Name)
declareFunction name kind loc = do
  CurrentScope currentScope' <- currentScope
  associatedScope <- newScope currentScope'
  node <- declareMaybeName name kind loc
  pure (node, associatedScope)

declareMaybeName ::
  StackGraphEff sig m =>
  Maybe Name ->
  Kind ->
  Loc ->
  m Stack.Node
declareMaybeName maybeName kind loc = do
  case maybeName of
    Just name -> declare name kind loc
    _ -> do
      name <- Name.gensym
      declare name kind loc

declareParameter :: StackGraphEff sig m => Name -> Int -> Kind -> Loc -> m Stack.Node
declareParameter n ix kind loc = do
  declNode <- declare n kind loc
  nameNode <- refer n kind loc
  indexNode <- refer (Name.nameI ix) kind loc
  let jumpNode = Stack.JumpToScope
  modify (Stack.addEdge declNode nameNode)
  modify (Stack.addEdge declNode indexNode)
  modify (Stack.addEdge nameNode jumpNode)
  modify (Stack.addEdge indexNode jumpNode)
  pure declNode

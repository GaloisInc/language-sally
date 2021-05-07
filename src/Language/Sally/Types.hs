{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Language.Sally.Types
-- Description : Abstract syntax tree for the Sally language
-- Copyright   : (c) Galois Inc, 2020
-- License     : BSD3
--
-- Maintainer  : valentin.robert.42@gmail.com
-- Stability   : experimental
-- |
module Language.Sally.Types
  ( -- * Name type
    Name,

    -- * Base types
    SallyBaseType,

    -- * Types for defining transition systems
    SallyState (..),
    SallyPred,
    SallyExpr,
    SallyStateFormula (..),
    SallyTransition (..),
    SallySystem (..),
    SallyQuery (..),
    mkSallyQuery,
    Sally (..),
  )
where

import Data.Functor.Const (Const (..))
import qualified Data.Parameterized.Context as Ctx
import Data.Text (Text)
import What4.Expr.Builder (Expr)
import qualified What4.Interface as What4

-- | For now, @Name@ is just an alias for @What4.SolverSymbol@
type Name = What4.SolverSymbol

-- | Base data types in Sally correspond to What4 base types
type SallyBaseType = What4.BaseTypeRepr

-- Typed Expression Abstract Syntax Tree for Sally --------------------------------------------

-- | A @SallyExpr@ expression is any expression of some type @What4.BaseType@
type SallyExpr t tp = Expr t tp

-- | A @SallyPred@ predicate is an expression with base type @What4.BaseBoolType@
type SallyPred t = Expr t What4.BaseBoolType

-- Compound Sally Types --------------------------------------------------------

-- | The state type in Sally
--
-- This consists of 1) a name for the type, 2) a set of state variables (and
-- their associated base type) and, 3) (optionally) a set in input variabels
-- which are uninterpreted in the model; they can be thought of as varying
-- non-deterministically in any system trace.
data SallyState stateType inputs = SallyState
  { -- | state type name
    sallyStateName :: Name,
    -- | state variables
    sallyStateVars :: Ctx.Assignment What4.BaseTypeRepr stateType,
    sallyStateVarsNames :: Ctx.Assignment (Const What4.SolverSymbol) stateType,
    -- | state input variables
    sallyStateInputs :: Ctx.Assignment What4.BaseTypeRepr inputs,
    sallyStateInputsNames :: Ctx.Assignment (Const What4.SolverSymbol) inputs
  }
  deriving (Show)

-- | A named formula over a state type
data SallyStateFormula t (stateType :: Ctx.Ctx What4.BaseType) = SallyStateFormula
  { -- | state formula name
    sallyStateFormulaName :: Name,
    -- | state formula domain
    sallyStateFormulaDomain :: Name,
    -- | state formula predicate
    sallyStateFormulaPredicate :: SallyPred t
  }

-- | A "let" binding: each let binds a 'SallyVar' to a Sally expression,
-- which can be a constant literal, a predicate (boolean value), or an
-- arithmetic expression.
-- type SallyLet = (SallyVar, SallyExpr)

-- | A @SallyTransition@ represents a single transition for a given state type.
data SallyTransition t = SallyTransition
  { -- | The domain of a transition is the state type on which it operates.
    transitionDomain :: Name,
    -- | The name of a transition is used to identify it.
    transitionName :: Name,
    -- | Bindings for the transition relation.
    -- traLet :: [SallyLet],
    -- | The transition relation is a predicate over the current and next states.
    transitionRelation :: SallyPred t
  }

-- | A transition system declaration
data SallySystem = SallySystem
  { -- | TODO
    sallySystemName :: Name,
    -- | TODO
    sallySystemStateName :: Name,
    -- | TODO
    sallySystemInitialStateName :: Name,
    -- | TODO
    sallySystemTransitionName :: Name
  }
  deriving (Show, Eq)

-- | Data structure for representing queries in Sally. A query consists of an
-- optional set of let bindings, followed by a predicate over those bindings.
data SallyQuery t = SallyQuery
  { -- | Name of the system to which this query applies
    sallyQuerySystemName :: Name,
    -- | let bindings
    -- sqLet :: [SallyLet],
    -- | predicate to query
    sallyQueryPredicate :: SallyPred t,
    -- | comment header for query
    sallyQueryComment :: Text
  }

-- | Convenience function to construct a minimal SallyQuery, omitting
-- any optional information.
mkSallyQuery ::
  -- | Name of the system to which this query applies
  Name ->
  -- [SallyLet] ->
  SallyPred t ->
  SallyQuery t
mkSallyQuery
  systemName
  -- lets
  queryPred =
    SallyQuery
      { sallyQuerySystemName = systemName,
        -- sqLet = lets,
        sallyQueryPredicate = queryPred,
        sallyQueryComment = ""
      }

-- | Representation of a Sally abstract syntax tree
data Sally t stateType inputs constants = Sally
  { -- | system state variables
    sallyState :: SallyState stateType inputs,
    -- | state formulas used in transitions
    --   and queries
    sallyFormulas :: [SallyStateFormula t stateType],
    -- | declared constants
    sallyConstants :: Ctx.Assignment (Expr t) constants,
    -- | initialization formula
    sallyInitialFormula :: SallyStateFormula t stateType,
    -- | system transitions
    sallyTransitions :: [SallyTransition t],
    -- | system definition
    sallySystem :: SallySystem,
    sallyQueries :: [SallyQuery t]
  }

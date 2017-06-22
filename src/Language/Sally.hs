-- |
-- Module      :  Language.Sally
-- Copyright   :  Benjamin Jones 2017
-- License     :  ISC
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- @language-sally@ is a package which contains a convenient AST and pretty
-- printer for the input language of SRI's Sally model checker
-- <https://github.com/SRI-CSL/sally>.
--
-- The main types exported by the package defined in "Language.Sally.Types"
-- and correspond to the main block types in Sally's input language: the state
-- type, state formula, transition formula, and transition system definition.
--
-- Low-level expressions and literals may be contructed using the functions in
-- "Language.Sally.Expr", while the pretty printing mechanism is split between
-- "Language.Sally.SExpPP" (dealing with layout of S-expressions) and
-- "Language.Sally.PPrint" (dealing with the rendering of ASTs).
--
-- This module re-exports all the definitions from the submodules.
--

module Language.Sally (
    -- * re-exports
    module Language.Sally.Expr
  , module Language.Sally.PPrint
  , module Language.Sally.SExpPP
  , module Language.Sally.Types
) where

import Language.Sally.Expr
import Language.Sally.PPrint
import Language.Sally.SExpPP
import Language.Sally.Types

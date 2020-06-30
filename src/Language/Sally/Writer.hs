{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Language.Sally.Writer
-- Description : Defines a What4 SMTWriter instance for outputting Sally programs
-- Copyright   : (c) Galois Inc, 2020
-- License     : BSD3
--
-- Maintainer  : valentin.robert.42@gmail.com
-- Stability   : experimental
-- |
module Language.Sally.Writer
  ( SallyReader (..),
    SallyWriter,
    newWriter,
    runSallyReader,
  )
where

import Control.Applicative
import Control.Monad.Reader
import qualified Data.Parameterized.Context as Ctx
import Data.Parameterized.Some
import Data.Parameterized.TraversableFC
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder
import qualified System.IO.Streams as Streams
import What4.Expr.Builder
import What4.ProblemFeatures
import What4.Protocol.SMTLib2 hiding (newWriter)
import What4.Protocol.SMTLib2.Syntax as SMT2 hiding
  ( abs,
    negate,
  )
import What4.Protocol.SMTWriter as SMTWriter hiding
  ( Term,
  )
import qualified What4.Protocol.SMTWriter as SMTWriter
import qualified What4.Symbol as What4
import Prelude hiding (init)

-- | What4 does not retain the mapping of variables in the context to their
-- name: it uses @Ctx.Index@ to safely reach for variables of a given type in a
-- @Ctx.Assignment@.
-- This has the unfortunate consequence that the formulas we get out of What4
-- are in terms of anonymous, numbered variables, rather than our original user
-- variables, making the output very hard to read.
--
-- Unfortunately, the context over which a What4 expression was defined is
-- existentially sealed in the current writer interface: we can not guarantee
-- that the term is indeed scoped over our original assignment.
--
-- For this reason, instead of keeping the @Ctx.Assignment What4.SolverSymbol
-- ctx@, it suffices to keep a simple list @[What4.SolverSymbol]@, within which
-- we can retrieve the user variable name by its index.
--
-- Finally, because the @SMTWriter@ interface is currently not parameterized
-- over a custom monad, our only way of threading this information all the way
-- down to where it is needed is to replace the plain output, usually a
-- @SMT2.Term@, with an enriched @Reader@, to be passed at the end.  We also
-- considered a comonadic @Env@, but we sometimes need to create values in
-- places where we don't have the list of symbols yet, so the "early-binding"
-- style of @Env@ is not applicable, and we need the "late-binding" style of
-- @Reader@.
newtype SallyReader a = SallyReader (Reader [What4.SolverSymbol] a)
  deriving (Functor)
  deriving (Applicative) via (Reader [What4.SolverSymbol])
  deriving (Monad) via (Reader [What4.SolverSymbol])
  deriving (MonadReader [What4.SolverSymbol]) via (Reader [What4.SolverSymbol])

runSallyReader :: [What4.SolverSymbol] -> SallyReader a -> a
runSallyReader symbols (SallyReader r) = runReader r symbols

data SallyWriter a = SallyWriter

type instance SMTWriter.Term (SallyWriter a) = SallyReader SMT2.Term

type instance SMTWriter.Command (SallyWriter a) = SallyReader SMT2.Command

varBinding ::
  forall a. SMTLib2Tweaks a => (Text, Some TypeMap) -> (Text, SMT2.Sort)
varBinding (nm, Some tp) = (nm, asSMT2Type @a tp)

-- Unfortunately, we need to tediously lift all the following instances.

instance Num (SallyReader SMT2.Term) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = fmap negate

instance SupportTermOps (SallyReader SMT2.Term) where
  boolExpr = pure . boolExpr
  notExpr = fmap notExpr
  andAll as = andAll <$> sequence as
  orAll as = orAll <$> sequence as
  fromText = pure . fromText
  (.==) = liftA2 (.==)
  integerTerm = pure . integerTerm
  rationalTerm = pure . rationalTerm
  letExpr lets body = letExpr <$> mapM sequence lets <*> body
  termIntegerToReal = fmap termIntegerToReal
  termRealToInteger = fmap termRealToInteger
  intAbs = fmap intAbs
  intDiv = liftA2 intDiv
  intMod = liftA2 intMod
  intDivisible t n = intDivisible <$> t <*> pure n
  bvTerm a b = pure (bvTerm a b)
  bvNeg = fmap bvNeg
  bvAdd = liftA2 bvAdd
  bvSub = liftA2 bvSub
  bvMul = liftA2 bvMul
  ite = liftA3 SMTWriter.ite
  (.<=) = liftA2 (.<=)
  bvSLe = liftA2 bvSLe
  bvURem = liftA2 bvURem
  bvSDiv = liftA2 bvSDiv
  bvSRem = liftA2 bvSRem
  bvAnd = liftA2 bvAnd
  bvOr = liftA2 bvOr
  bvXor = liftA2 bvXor
  bvNot = fmap bvNot
  bvShl = liftA2 bvShl
  bvLshr = liftA2 bvLshr
  bvAshr = liftA2 bvAshr
  bvConcat = liftA2 bvConcat
  bvExtract w a b = fmap (bvExtract w a b)
  bvULe = liftA2 bvULe
  bvSLt = liftA2 bvSLt
  bvULt = liftA2 bvULt
  bvUDiv = liftA2 bvUDiv
  floatPZero = pure . floatPZero
  floatNZero = pure . floatNZero
  floatNaN = pure . floatNaN
  floatPInf = pure . floatPInf
  floatNInf = pure . floatNInf
  floatNeg = fmap floatNeg
  floatAbs = fmap floatAbs
  floatSqrt rm = fmap (floatSqrt rm)
  floatAdd rm = liftA2 (floatAdd rm)
  floatSub rm = liftA2 (floatSub rm)
  floatMul rm = liftA2 (floatMul rm)
  floatDiv rm = liftA2 (floatDiv rm)
  floatRem = liftA2 floatRem
  floatMin = liftA2 floatMin
  floatMax = liftA2 floatMax
  floatFMA rm = liftA3 (floatFMA rm)
  floatEq = liftA2 floatEq
  floatFpEq = liftA2 floatFpEq
  floatLe = liftA2 floatLe
  floatLt = liftA2 floatLt
  floatIsNaN = fmap floatIsNaN
  floatIsInf = fmap floatIsInf
  floatIsZero = fmap floatIsZero
  floatIsPos = fmap floatIsPos
  floatIsNeg = fmap floatIsNeg
  floatIsSubnorm = fmap floatIsSubnorm
  floatIsNorm = fmap floatIsNorm
  floatCast p rm = fmap (floatCast p rm)
  floatRound rm = fmap (floatRound rm)
  floatFromBinary p = fmap (floatFromBinary p)
  bvToFloat p rm = fmap (bvToFloat p rm)
  sbvToFloat p rm = fmap (sbvToFloat p rm)
  realToFloat p rm = fmap (realToFloat p rm)
  floatToBV n rm = fmap (floatToBV n rm)
  floatToSBV n rm = fmap (floatToSBV n rm)
  floatToReal = fmap floatToReal
  realIsInteger = fmap realIsInteger
  realDiv = liftA2 realDiv
  realSin = fmap realSin
  realCos = fmap realCos
  realATan2 = liftA2 realATan2
  realSinh = fmap realSinh
  realCosh = fmap realCosh
  realExp = fmap realExp
  realLog = fmap realLog
  smtFnApp f as = smtFnApp <$> f <*> sequence as

-- | The @SMTWriter@ instance for @SallyWriter@ is where we inject
-- Sally-specific tweaks.
--
-- Sally has the following features, which we need cater for:
--
-- 1. Structures are not declared in the usual SMT style.  Instead, a "state
-- type" is declared for the transition system, and is likely the only structure
-- allowed.  As such, we discard all type declaration attempts by the writer.
--
-- 2. Because of our @SallyReader@ hack, every @SMTWriter.Term@ is actually a
-- @SMT2.Term@-producing @Reader@, which needs to be accounted for everywhere.
--
-- 3. While the initial state formula of a Sally system ranges over a structure
-- with the state type, the formula should not mention the structure, and can
-- just use the field names, as it is implicit that the initial formula talks
-- about the initial state.  In our What4 encoding, it is much easier to
-- actually reify the initial state, which we name "init", and so the writer
-- must substitute instances of @init.field@ with simply a naked @field@.  This
-- is done in @structProj@.
instance SMTLib2Tweaks a => SMTWriter (SallyWriter a) where
  forallExpr vars t = SMT2.forall (varBinding @a <$> vars) <$> t
  existsExpr vars t = SMT2.exists (varBinding @a <$> vars) <$> t

  arrayConstant =
    case smtlib2arrayConstant @a of
      Just f -> Just $ \idxTypes (Some retType) c ->
        f ((\(Some itp) -> asSMT2Type @a itp) <$> idxTypes) (asSMT2Type @a retType) <$> c
      Nothing -> Nothing
  arraySelect = \t ts -> smtlib2arraySelect @a <$> t <*> sequence ts
  arrayUpdate = \t ts t' -> smtlib2arrayUpdate @a <$> t <*> sequence ts <*> t'

  commentCommand _ b = pure (SMT2.Cmd ("; " <> b))

  assertCommand _ t = do
    T e <- t
    pure (SMT2.Cmd ("; assertCommand: " <> e))

  assertNamedCommand _ e nm = SMT2.assertNamed <$> e <*> pure nm

  pushCommand _ = pure $ SMT2.push 1
  popCommand _ = pure $ SMT2.pop 1
  resetCommand _ = pure SMT2.resetAssertions

  checkCommands _ = [pure SMT2.checkSat]
  checkWithAssumptionsCommands _ nms =
    [pure $ SMT2.checkSatWithAssumptions nms]

  getUnsatAssumptionsCommand _ = pure SMT2.getUnsatAssumptions
  getUnsatCoreCommand _ = pure SMT2.getUnsatCore
  setOptCommand _ = (pure <$>) <$> SMT2.setOption

  declareCommand _proxy v argTypes retType =
    let Cmd declaration =
          SMT2.declareFun
            v
            (toListFC (asSMT2Type @a) argTypes)
            (asSMT2Type @a retType)
     in pure $ SMT2.Cmd ("; declareCommand: " <> declaration)

  defineCommand _proxy f args return_type e =
    let resolveArg (var, Some tp) = (var, asSMT2Type @a tp)
     in SMT2.defineFun f (resolveArg <$> args) (asSMT2Type @a return_type) <$> e

  stringTerm bs = pure $ smtlib2StringTerm @a bs
  stringLength x = smtlib2StringLength @a <$> x
  stringAppend xs = smtlib2StringAppend @a <$> sequence xs
  stringContains x y = smtlib2StringContains @a <$> x <*> y
  stringIsPrefixOf x y = smtlib2StringIsPrefixOf @a <$> x <*> y
  stringIsSuffixOf x y = smtlib2StringIsSuffixOf @a <$> x <*> y
  stringIndexOf x y k = smtlib2StringIndexOf @a <$> x <*> y <*> k
  stringSubstring x off len = smtlib2StringSubstring @a <$> x <*> off <*> len

  structCtor _tps vals = smtlib2StructCtor @a <$> sequence vals

  structProj _tps idx v = do
    let i = Ctx.indexVal idx
    names <- ask
    let field = Builder.fromString (show (names !! i))
    struct <- renderTerm <$> v
    pure $
      -- NOTE: We reserved the "init" and "query" namespaces as having a special
      -- meaning.  This is necessary for getting What4 to not anonymize field
      -- accesses.
      if struct `elem` ["init", "query"]
      then T field
      else T (struct <> "." <> field)

  resetDeclaredStructs _conn = unhandled "resetDeclaredStructs"

  declareStructDatatype _conn _flds =
    -- sally has its own way of declaring the state type
    -- unclear whether one can define other struct datatypes
    pure ()

  -- NOTE: at the moment, the commands SMTLib2 wants to output are pointless for
  -- sally, so we just silently skip them.
  writeCommand conn cmd =
    if False -- for debugging purposes
      then do
        let Cmd c = runSallyReader [] cmd
        let cmdout = Lazy.toStrict (Builder.toLazyText c)
        Streams.write (Just (cmdout <> "\n")) (connHandle conn)
        -- force a flush
        Streams.write (Just "") (connHandle conn)
      else pure ()

unhandled :: String -> IO a
unhandled construct = error $ "Unhandled by language-sally: " ++ construct ++ ", please report to maintainers."

newWriter ::
  a ->
  Streams.OutputStream Text ->
  -- | Action to run for consuming acknowledgement messages
  AcknowledgementAction t (SallyWriter a) ->
  -- | Name of solver for reporting purposes.
  String ->
  -- | Indicates what level of arithmetic is supported by solver.
  ProblemFeatures ->
  -- | Indicates if quantifiers are supported.
  SymbolVarBimap t ->
  IO (WriterConn t (SallyWriter a))
newWriter _ h ack solver_name arithOption bindings = do
  let initWriter = SallyWriter
  conn <- newWriterConn h ack solver_name arithOption bindings initWriter
  return $! conn {supportFunctionDefs = False, supportQuantifiers = False}

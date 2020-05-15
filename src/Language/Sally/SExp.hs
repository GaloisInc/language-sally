{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Language.Sally.SExp
-- Description : A simple S-expression type with a pretty printable 'Doc' type
--               at the leaves
-- Copyright   : (c) Galois Inc, 2020
-- License     : BSD3
--
-- Maintainer  : valentin.robert.42@gmail.com
-- Stability   : experimental
-- |
module Language.Sally.SExp
  ( -- * S-expression pretty-printing
    SExp (..),
    sexpOfText,
    sexpOfBaseTypeRepr,
    sexpOfExpr,
    sexpOfPred,
    sexpOfSally,
    sexpToCompactDoc,
    sexpToDoc,
  )
where

import Control.Monad.Reader
import Data.Bifunctor
import Data.Functor.Const
import Data.List (intersperse)
import qualified Data.Parameterized.Context as Ctx
import Data.Parameterized.TraversableFC
  ( toListFC,
    traverseFC,
  )
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
  ( Builder,
    toLazyText,
  )
import Language.Sally.Types
import Language.Sally.Writer
import qualified System.IO.Streams as Streams
import Text.PrettyPrint.ANSI.Leijen hiding
  ( (<$>),
  )
import What4.Expr.Builder
import qualified What4.Interface as What4
import qualified What4.Protocol.SMTLib2 as SMT2
import What4.Protocol.SMTWriter as SMTWriter hiding
  ( Term,
  )
import What4.Solver.Adapter (defaultWriteSMTLIB2Features)
import What4.Symbol

-- | A simple S-expression datatype with 'Doc' values at the leaves.
data SExp
  = -- | Bare symbol or literal represented by a 'Doc'
    SXBare Doc
  | -- | list of 'SExp', e.g. (foo a b)
    SXList [SExp]

sexpOfBuilder :: Builder -> SExp
sexpOfBuilder = SXBare . text . T.unpack . toStrict . toLazyText

sexpOfBaseTypeRepr :: WriterConn t h -> What4.BaseTypeRepr bt -> IO SExp
sexpOfBaseTypeRepr conn bt = do
  case SMTWriter.typeMap conn bt of
    Left _e ->
      error $
        "ppSallyBaseType: could not map type "
          ++ show bt
          ++ ", please report:\n"
    Right tm -> do
      let s = SMT2.asSMT2Type @() tm
      pure $ sexpOfBuilder $ SMT2.unSort s

withLets :: [(SExp, SExp)] -> SExp -> SExp
withLets [] expr = expr
withLets bindings expr = SXList [sexpOfText "let", SXList sexpBindings, expr]
  where
    sexpBindings = map (\(v, e) -> SXList [v, e]) bindings

sexpOfText :: T.Text -> SExp
sexpOfText = SXBare . text . T.unpack

sexpOfTerm :: SMT2.Term -> SExp
sexpOfTerm = sexpOfBuilder . SMT2.renderTerm

sexpOfExpr ::
  MonadIO m =>
  MonadReader [What4.SolverSymbol] m =>
  WriterConn sym (SallyWriter ()) ->
  Expr sym bt ->
  m SExp
sexpOfExpr conn expr = do
  fields <- ask
  liftIO $ do
    CollectorResults {crBindings, crResult} <- runInSandbox conn $ do
      SallyReader r <- mkBaseExpr expr
      return (runReader r fields)
    pure $
      withLets
        (map (bimap sexpOfText (sexpOfTerm . runSallyReader fields)) crBindings)
        (sexpOfTerm crResult)

-- | @sexpOfPred@ is an alias for @sexpOfExpr@ restricted to @What4.Pred@, that
-- is, expressions returning a boolean.
sexpOfPred ::
  MonadIO m =>
  MonadReader [What4.SolverSymbol] m =>
  WriterConn sym (SallyWriter ()) ->
  Expr sym What4.BaseBoolType ->
  m SExp
sexpOfPred = sexpOfExpr

sexpOfSymbol :: What4.SolverSymbol -> SExp
sexpOfSymbol = SXBare . text . T.unpack . solverSymbolAsText

-- | Pretty print an 'SExp' using the default layout scheme.
sexpToDoc :: SExp -> Doc
sexpToDoc (SXBare x) = x
sexpToDoc (SXList []) = lparen <> rparen
sexpToDoc (SXList xs) = parens . group . align . vsep . fmap sexpToDoc $ xs

-- sxPrettyDefault (SXList ll@(x:_)) = case x of
--   SXBare _ -> parens (hang' (fillSep (map sxPretty ll)))
--   SXList _ -> parens (fillSep (map sxPretty ll))

-- | Pretty print an 'SExp' using the default *compact* layout scheme.
sexpToCompactDoc :: SExp -> Doc
sexpToCompactDoc (SXBare x) = x
sexpToCompactDoc (SXList []) = lparen <> rparen
sexpToCompactDoc (SXList xs) = parens . hsep . fmap sexpToDoc $ xs

-- | A Sally comment
sallyComment :: Doc
sallyComment = text ";;"

sexpsOfNamedContext ::
  WriterConn t h ->
  Ctx.Assignment What4.BaseTypeRepr ctx ->
  Ctx.Assignment (Const What4.SolverSymbol) ctx ->
  IO [SExp]
sexpsOfNamedContext conn types names =
  toListFC getConst <$> Ctx.zipWithM sexp types names
  where
    sexp ::
      What4.BaseTypeRepr tp ->
      Const What4.SolverSymbol tp ->
      IO (Const SExp tp)
    sexp t (Const n) = do
      t' <- sexpOfBaseTypeRepr conn t
      pure (Const (SXList [sexpOfSymbol n, t']))

sexpOfSallyState :: WriterConn t h -> SallyState stateType inputs -> IO SExp
sexpOfSallyState
  conn
  SallyState
    { sallyStateInputs,
      sallyStateInputsNames,
      sallyStateName,
      sallyStateVars,
      sallyStateVarsNames
    } =
    do
      sv <- sexpsOfNamedContext conn sallyStateVars sallyStateVarsNames
      si <- sexpsOfNamedContext conn sallyStateInputs sallyStateInputsNames
      pure
        $ SXList
        $ [sexpOfText "define-state-type", sexpOfSymbol sallyStateName, SXList sv]
          ++ (if null si then [] else [SXList si])

sexpOfSallyStateFormula ::
  MonadIO m =>
  MonadReader [What4.SolverSymbol] m =>
  WriterConn t (SallyWriter ()) ->
  SallyStateFormula t stateType ->
  m SExp
sexpOfSallyStateFormula
  conn
  SallyStateFormula
    { sallyStateFormulaDomain,
      sallyStateFormulaName,
      sallyStateFormulaPredicate
    } = do
    p <- sexpOfExpr conn sallyStateFormulaPredicate
    pure $
      SXList
        [sexpOfText "define-states", sexpOfSymbol sallyStateFormulaName, sexpOfSymbol sallyStateFormulaDomain, p]

sexpOfSallyTransition ::
  MonadIO m =>
  MonadReader [What4.SolverSymbol] m =>
  WriterConn t (SallyWriter ()) ->
  SallyTransition t ->
  m SExp
sexpOfSallyTransition conn SallyTransition {transitionName, transitionDomain, transitionRelation} =
  do
    p <- sexpOfPred conn transitionRelation
    pure
      $ SXList
      $ [ sexpOfText "define-transition",
          sexpOfSymbol transitionName,
          sexpOfSymbol transitionDomain,
          p -- TODO: lets, cf. comment below
        ]

-- ++ ( if null listOfBinds
--        then [toSExp tp]
--        else [SXList [sexpOfText "let", SXList listOfBinds, toSExp tp]]
--    )

-- where
-- listOfBinds = map (\(v, e) -> SXList [toSExp v, toSExp e]) tl

sexpOfSallySystem :: SallySystem -> SExp
sexpOfSallySystem
  SallySystem
    { sallySystemName,
      sallySystemStateName,
      sallySystemInitialStateName,
      sallySystemTransitionName
    } =
    SXList
      [ sexpOfText "define-transition-system",
        sexpOfSymbol sallySystemName,
        sexpOfSymbol sallySystemStateName,
        sexpOfSymbol sallySystemInitialStateName,
        sexpOfSymbol sallySystemTransitionName
      ]

sexpOfSallyQuery ::
  MonadIO m =>
  MonadReader [What4.SolverSymbol] m =>
  WriterConn sym (SallyWriter ()) ->
  SallyQuery sym ->
  m SExp
sexpOfSallyQuery
  conn
  SallyQuery
    { -- sallyQueryComment,
      sallyQueryPredicate,
      sallyQuerySystemName
    } = do
    queryPredicate <- sexpOfPred conn sallyQueryPredicate
    pure $ SXList $
      [ sexpOfText "query",
        sexpOfSymbol sallyQuerySystemName,
        queryPredicate
      ]

-- | Sally requires a special printer since it is not an s-expression. The
-- order of the 'vcat' items is important because Sally is sensitive to names
-- being declared before they are used in a model file.
sexpOfSally ::
  ExprBuilder t st fs ->
  Sally t stateType inputs constants ->
  IO SExp
sexpOfSally
  sym
  Sally
    { sallyConstants,
      sallyFormulas,
      sallyInitialFormula,
      sallyQueries,
      sallyState,
      sallySystem,
      sallyTransitions
    } = do
    bindings <- getSymbolVarBimap sym
    inStr <- Streams.encodeUtf8 Streams.stderr
    conn <-
      liftIO $
        newWriter
          ()
          inStr
          nullAcknowledgementAction
          "NoSolver"
          defaultWriteSMTLIB2Features
          bindings
    let symbols = toListFC getConst (sallyStateVarsNames sallyState)
    flip runReaderT symbols $ do
      state <- liftIO $ sexpOfSallyState conn sallyState
      formulas <- mapM (sexpOfSallyStateFormula conn) sallyFormulas
      initial <- sexpOfSallyStateFormula conn sallyInitialFormula
      transitions <- mapM (sexpOfSallyTransition conn) sallyTransitions
      let system = sexpOfSallySystem sallySystem
      queries <- mapM (sexpOfSallyQuery conn) sallyQueries
      constants <- traverseFC ((Const <$>) . sexpOfExpr conn) sallyConstants
      let ppConsts = toListFC getConst constants
      let consts =
            if null ppConsts
              then text ";; NONE"
              else vcat (sexpToDoc <$> ppConsts)
      pure
        $ SXBare
        $ vcat [consts_comment, consts, state_comment, sexpToDoc state]
          <$$> vcat
            ( formulas_comment
                : intersperse sallyComment (map sexpToDoc formulas)
            )
          <$$>
          -- needs to come after formulas
          vcat [init_comment, sexpToDoc initial]
          <$$>
          -- needs to come after state, init, and formulas
          vcat
            ( trans_comment
                : intersperse sallyComment (map sexpToDoc transitions)
            )
          <$$>
          -- needs to come (almost) last
          vcat (system_comment : [sexpToDoc system])
          <$$>
          -- queries
          vcat (queries_comment : map sexpToDoc queries)
    where
      consts_comment = sallyComment <+> text "Constants"
      state_comment = linebreak <> sallyComment <+> text "State type"
      init_comment = linebreak <> sallyComment <+> text "Initial State"
      formulas_comment = linebreak <> sallyComment <+> text "State Formulas"
      trans_comment = linebreak <> sallyComment <+> text "Transitions"
      system_comment = linebreak <> sallyComment <+> text "System Definition"
      queries_comment = linebreak <> sallyComment <+> text "Queries"

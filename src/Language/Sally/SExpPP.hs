-- |
-- Module      :  SExpPP
-- Description :  A simple S-expression type with a pretty printable 'Doc' type
--                at the leaves
-- Copyright   :  Benjamin F Jones 2016
-- License     :  ISC
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- This module gives a uniform way to pretty print S-expressions through a
-- typeclass 'ToSExp'.
--

{-# LANGUAGE OverloadedStrings #-}

module Language.Sally.SExpPP (
  -- * S-expression pretty printing
    SExp(..)
  , ToSExp(..)
  , bareText
  -- * misc
  , sallyCom
) where


import Data.Text.Lazy (Text)
import Text.PrettyPrint.Leijen.Text


-- | A simple S-expression datatype with 'Doc' values at the leaves.
data SExp = SXBare Doc     -- ^ bare symbol or literal represented by a 'Doc'
          | SXList [SExp]  -- ^ list of 'SExp', e.g. (foo a b)

-- | Typeclass for values that can be converted to a 'SExp'. These values can
-- then be pretty printed using the default layout scheme given by 'sxPretty'.
class ToSExp a where
  toSExp :: a -> SExp

  sxPretty :: a -> Doc
  sxPretty = sxPrettyDefault . toSExp

-- | Trivial 'ToSExp' instance for 'SExp'.
instance ToSExp SExp where
  toSExp = id

-- | Pretty print an 'SExp' using the default layout scheme.
sxPrettyDefault :: SExp -> Doc
sxPrettyDefault (SXBare x) = x
sxPrettyDefault (SXList []) = lparen <> rparen
sxPrettyDefault (SXList xs) = parens . group . align . vsep . fmap sxPretty $ xs
-- sxPrettyDefault (SXList ll@(x:_)) = case x of
--   SXBare _ -> parens (hang' (fillSep (map sxPretty ll)))
--   SXList _ -> parens (fillSep (map sxPretty ll))

-- | Inject a text literal as a 'SExp'.
bareText :: Text -> SExp
bareText = SXBare . text


-- Misc Sally Specific Items ---------------------------------------------------

sallyCom :: Doc
sallyCom = text ";;"

-- |
-- Module      :  Language.Sally.PPrint
-- Description :  Export some pretty printer utilities
-- Copyright   :  Benjamin Jones 2017
-- License     :  ISC
--
-- Maintainer  :  bjones@galois.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Export top-level pretty printer functions taking general pretty-printable
-- values and writing them to 'stdout' or to 'String'.
--

module Language.Sally.PPrint (
  -- * pretty printing
    spPrint
  , pprintSystem
  , putSystem
  , putSystemCompact
  , putSystemLn
  , hPutSystem
) where

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.ByteString.Lazy.Char8 as BS

import System.IO (Handle)
import Text.PrettyPrint.Leijen.Text


spPrint :: Pretty a => a -> String
spPrint = L.unpack . pprintSystem

pprintSystem :: Pretty a => a -> L.Text
pprintSystem = displayT . renderPretty ribbon wid . pretty
  where ribbon = 72 / 80 :: Float
        wid    = 80

putSystem :: Pretty a => a -> IO ()
putSystem = putDoc . pretty

putSystemCompact :: Pretty a => a -> IO ()
putSystemCompact = putDoc . sxPrettyCompact

putSystemLn :: Pretty a => a -> IO ()
putSystemLn tr = putSystem tr >> putStrLn ""

hPutSystem :: Pretty a => Handle -> a -> IO ()
hPutSystem h = BS.hPutStr h . E.encodeUtf8 . pprintSystem

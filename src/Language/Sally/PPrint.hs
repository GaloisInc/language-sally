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
-- values and writing them to 'stdout' or to 'String'. These functions avoid
-- the need to export the specific pretty printer library to clients.
--

module Language.Sally.PPrint (
  -- * pretty printing
    spPrint
  , pprintSystem
  , putSystem
  , putSExpCompact
  , putSystemLn
  , hPutSystem
) where

import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as BS

import System.IO (Handle)
import Text.PrettyPrint.ANSI.Leijen

import Language.Sally.SExpPP


-- TODO Rename and prune these functions.

-- | Render a value of the 'Pretty' class as a string.
spPrint :: Pretty a => a -> String
spPrint = T.unpack . pprintSystem

-- | Render a value of the 'Pretty' class as "Data.Text".
pprintSystem :: Pretty a => a -> Text
pprintSystem x = T.pack (displayS (renderPretty ribbon wid . pretty $ x) "")
  where ribbon = 72 / 80 :: Float
        wid    = 80

-- | Render a value of the pretty class to 'stdout'.
putSystem :: Pretty a => a -> IO ()
putSystem = putDoc . pretty

-- | Render a value of the pretty class in a compact fashion to 'stdout'.
putSExpCompact :: ToSExp a => a -> IO ()
putSExpCompact = putDoc . sxPrettyCompact

-- | Same as 'putSystem' but with a newline.
putSystemLn :: Pretty a => a -> IO ()
putSystemLn tr = putSystem tr >> putStrLn ""

-- | Same as 'putSystem' but takes a 'Handle'.
hPutSystem :: Pretty a => Handle -> a -> IO ()
hPutSystem h = BS.hPutStr h . E.encodeUtf8 . pprintSystem

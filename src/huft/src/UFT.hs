module UFT where

import Languages
import Control.Exception (Exception, throw)
import Data.ByteString (hGetContents, hPutStr)
import Text.Parsec (runParser)
import qualified AsmParse
import qualified AsmUnparse

-- support for materialization

data InternalError = Backward
                   | NoTranslationTo Language
                  deriving (Show)
instance Exception InternalError

-- Reader functions

vsOfFile infile = do
  contents <- hGetContents infile
  return (runParser AsmParse.asmParse () "placeholder" contents)

-- Materializer functions

vsOf VS = vsOfFile
vsOf _ = throw (NoTranslationTo VS)

-- Emitter functions

-- Universal Forward Translator

data UFTException = NotForward Language Language
  deriving (Show)
instance Exception UFTException

-- translate inLang outLang infile outfile =
--   case outLang of
--     VS -> do
--       a <- vsOf inLang infile
--       case a of
--         Left e -> putStrLn "a"
--         Right r -> hPutStr $ (AsmUnparse.unparse r)


module UFT where

import Languages
import Control.Exception (Exception, throw)
import Text.Parsec (runParser, ParseError)
import qualified AsmParse
import qualified AsmUnparse
import GHC.IO.Handle (hGetContents', hPutStr, Handle)
import Asm (Instr(ObjectCode))
import qualified ObjectUnparser as ObjectCode
import Text.Parsec.String (Parser)

newtype ParseException = ParseException ParseError deriving (Show)
instance Exception ParseException

parseAndExcept :: Parser a -> String -> a
parseAndExcept p input = case runParser p () "" input of
  Left e -> throw (ParseException e)
  Right r -> r

-- support for materialization

data InternalError = Backward
                   | NoTranslationTo Language
                  deriving (Show)
instance Exception InternalError

-- Reader functions

vsOfFile infile = parseAndExcept AsmParse.asmParse <$> hGetContents' infile

-- Materializer functions

vsOf VS = vsOfFile
vsOf _ = throw (NoTranslationTo VS)

-- Emitter functions

emitVO outfile = mapM_ (hPutStr outfile) . ObjectCode.unparseModule

emitVS outfile = mapM_ (hPutStr outfile) . AsmUnparse.unparse

-- Universal Forward Translator

data UFTException = NotForward Language Language
  deriving (Show)
instance Exception UFTException

translate :: Language -> Language -> Handle -> Handle -> IO ()
translate inLang outLang infile outfile =
  case outLang of
    VS -> vsOf inLang infile >>= emitVS outfile


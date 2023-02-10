module UFT where

import Languages
import Control.Exception (Exception, throw)
import Text.Parsec (runParser, ParseError)

import GHC.IO.Handle (hGetContents', hPutStr, Handle)
import Asm (Instr(ObjectCode))
import Text.Parsec.String (Parser)
import System.IO (hPutStrLn)
import Error ( Error )
import Data.Functor ((<&>))

import qualified Asm
import qualified AsmParse
import qualified AsmUnparse
import qualified ObjectCode
import qualified ObjectUnparser

type Emitter a = Handle -> a -> IO ()
type Reader a = Handle -> IO (Error a)

(==>) :: Reader a -> (a -> Error b) -> Reader b
(r ==> f) h = r h <&> (f =<<)

parseAndErr :: Parser a -> String -> Error a
parseAndErr p input = case runParser p () "" input of
  Left e -> Left (show e)
  Right r -> Right r

-- support for materialization

data InternalError = Backward
                   | NoTranslationTo Language
                  deriving (Show)
instance Exception InternalError

-- Reader functions

vsOfFile :: Reader [Asm.Instr]
vsOfFile infile = parseAndErr AsmParse.asmParse <$> hGetContents' infile

-- create (Reader a) -> (a -> Error b) -> Reader b infix op

-- Materializer functions

vsOf :: Language -> Reader [Asm.Instr]
vsOf VS = vsOfFile
vsOf _ = throw (NoTranslationTo VS)

-- Emitter functions

emitVO :: Emitter [ObjectCode.Instr]
emitVO outfile = mapM_ (hPutStrLn outfile) . ObjectUnparser.unparseModule

emitVS :: Emitter [Asm.Instr]
emitVS outfile = mapM_ (hPutStrLn outfile) . AsmUnparse.unparse

-- Universal Forward Translator

data UFTException = NotForward Language Language
  deriving (Show)
instance Exception UFTException

translate :: (Language, Language) -> (Handle, Handle) -> IO (Error (IO ()))
translate (inLang, outLang) (infile, outfile) =
  case outLang of
    VS -> vsOf inLang infile <&> (emitVS outfile <$>)
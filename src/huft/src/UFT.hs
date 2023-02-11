module UFT where

import Languages
import Control.Exception (Exception, throw)
import Text.Parsec (runParser, ParseError)

import GHC.IO.Handle (hGetContents', hPutStr, Handle)
import Text.Parsec.String (Parser)
import System.IO (hPutStrLn)
import Error ( Error )
import Data.Functor ((<&>))

import qualified Asm
import qualified AsmParse
import qualified AsmUnparse
import qualified ObjectCode
import qualified ObjectUnparser
import qualified Assembler

type Emitter a = Handle -> a -> IO ()
type Reader a = Handle -> IO (Error a)

-- (<&>) = flip fmap
-- Just 2 <&> (+1)
-- fmap (+1) Just 2
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
-- vsOfFile Handle -> IO (Error [Asm.Instr])
vsOfFile infile = hGetContents' infile <&> parseAndErr AsmParse.asmParse 

-- Materializer functions

vsOf :: Language -> Reader [Asm.Instr]
vsOf VS = vsOfFile
vsOf _ = throw (NoTranslationTo VS)

voOf :: Language -> Reader [ObjectCode.Instr]
voOf VO     =  throw (NoTranslationTo VO)
voOf inLang =  vsOf inLang ==> Assembler.translate

-- Emitter functionsz

emitVO :: Emitter [ObjectCode.Instr]
emitVO outfile = mapM_ (hPutStrLn outfile) . ObjectUnparser.unparseModule

emitVS :: Emitter [Asm.Instr]
emitVS outfile = mapM_ (hPutStrLn outfile) . AsmUnparse.unparse

-- Universal Forward Translator

data UFTException = NotForward Language Language
  deriving (Show)
instance Exception UFTException

translate :: Language -> Language -> Handle -> Handle -> IO (Error (IO ()))
translate inLang outLang infile outfile =
  case outLang of
    VS -> vsOf inLang infile <&> (emitVS outfile <$>)
    VO -> voOf inLang infile <&> (emitVO outfile <$>)

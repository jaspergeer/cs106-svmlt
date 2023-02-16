module UFT where

import Languages
import Control.Exception (Exception, throw, catch)
import Text.Parsec (runParser, ParseError)

import GHC.IO.Handle (hGetContents', hPutStr, Handle)
import Text.Parsec.String (Parser)
import System.IO (hPutStrLn)
import Error
import Data.Functor ((<&>))

import qualified Error as E
import qualified Asm
import qualified AsmParse
import qualified AsmUnparse
import qualified ObjectCode
import qualified ObjectUnparser
import qualified Assembler

type Emitter a = Handle -> a -> IO ()
type Reader a = Handle -> IO (E.Error a)

-- (<&>) = flip fmap
-- Just 2 <&> (+1)
-- fmap (+1) Just 2
(==>) :: Reader a -> (a -> E.Error b) -> Reader b
(r ==> f) h = r h <&> (f =<<)

parseAndErr :: Parser a -> String -> E.Error a
parseAndErr p input = case runParser p () "" input of
  Left e -> ERROR $ E.Error $ Left (show e)
  Right r -> ERROR $ E.Error $ Right r

-- support for materialization

data InternalException = Backward
                       | NoTranslationTo Language
                  deriving (Show)
instance Exception InternalException

-- Reader functions

vsOfFile :: Reader [Asm.Instr]
vsOfFile infile = hGetContents' infile <&> parseAndErr AsmParse.asmParse 

-- Materializer functions

vsOf :: Language -> Reader [Asm.Instr]
vsOf VS = vsOfFile
vsOf _ = throw (NoTranslationTo VS)

voOf :: Language -> Reader [ObjectCode.Instr]
voOf VO     =  \_ -> return (ERROR $ Left "There is no reader for .vo")
voOf inLang =  vsOf inLang ==> Assembler.translate

-- Emitter functions

emitFrom :: (a -> [String]) -> Emitter a
emitFrom f outfile = mapM_ (hPutStrLn outfile) . f

emitVO :: Emitter [ObjectCode.Instr]
emitVO = emitFrom ObjectUnparser.unparseModule

emitVS :: Emitter [Asm.Instr]
emitVS = emitFrom AsmUnparse.unparse

-- Universal Forward Translator

data UFTException = NotForward Language Language
  deriving (Show)
instance Exception UFTException

translate :: Language -> Language -> Handle -> Handle -> IO (E.Error (IO ()))
translate inLang outLang infile outfile = catch
  (case outLang of
    VS -> vsOf inLang infile <&> (emitVS outfile <$>)
    VO -> voOf inLang infile <&> (emitVO outfile <$>))
  (\e -> case e of
    Backward -> throw (NotForward inLang outLang)
    NoTranslationTo l -> throw (NotForward inLang l))

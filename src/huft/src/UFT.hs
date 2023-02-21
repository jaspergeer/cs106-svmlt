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
import qualified UnambiguousVScheme
import qualified VScheme
import qualified VSchemeParse
import qualified VSchemeUnparse
import qualified Disambiguate
import qualified Ambiguate
import qualified KNF
import qualified KnEmbed
import qualified KnProject

type Emitter a = Handle -> a -> IO ()
type Reader a = Handle -> IO (E.Error a)

-- (<&>) = flip fmap
-- Just 2 <&> (+1)
-- fmap (+1) Just 2
(==>) :: Reader a -> (a -> E.Error b) -> Reader b
(r ==> f) h = r h <&> (f =<<)

parseAndErr :: Parser a -> String -> E.Error a
parseAndErr p input = case runParser p () "" input of
  Left e -> E.Error $ Left (show e)
  Right r -> E.Error $ Right r

-- support for materialization

data InternalException = Backward
                       | NoTranslationTo Language
                  deriving (Show)
instance Exception InternalException

-- Reader functions 

schemeOfFile :: Reader [VScheme.Def]
schemeOfFile infile = hGetContents' infile <&> parseAndErr VSchemeParse.parse

schemexOfFile :: Reader [UnambiguousVScheme.Def]
schemexOfFile = schemeOfFile ==> ((E.Error . Right) . map Disambiguate.disambiguate)

vsOfFile :: Reader [Asm.Instr]
vsOfFile infile = hGetContents' infile <&> parseAndErr AsmParse.parse

knOfFile :: Reader [KNF.Exp KNF.Name]
knOfFile = schemexOfFile ==> mapM KnProject.def  -- projection HO -> KN

-- Materializer functions

hoOf :: Language -> Reader [UnambiguousVScheme.Def]
hoOf HO = schemexOfFile
hoOf HOX = error "imperative features (HOX to HO)"
hoOf _ = throw Backward

vsOf :: Language -> Reader [Asm.Instr]
vsOf VS = vsOfFile
vsOf _ = throw (NoTranslationTo VS)

voOf :: Language -> Reader [ObjectCode.Instr]
voOf VO     =  \_ -> return (Error $ Left "There is no reader for .vo")
voOf inLang =  vsOf inLang ==> Assembler.translate

knTextOf :: Language -> Reader [KNF.Exp KNF.Name]
knTextOf KN = knOfFile
knTextOf _ = error "bad :("

-- Emitter functions

emitFrom :: (a -> [String]) -> Emitter a
emitFrom f outfile = mapM_ (hPutStrLn outfile) . f

emitVO :: Emitter [ObjectCode.Instr]
emitVO = emitFrom ObjectUnparser.unparseModule

emitVS :: Emitter [Asm.Instr]
emitVS = emitFrom AsmUnparse.unparse

emitScheme :: Emitter [VScheme.Def]
emitScheme = emitFrom (map VSchemeUnparse.pp)

emitHO :: Emitter [UnambiguousVScheme.Def]
emitHO outfile = emitScheme outfile . map Ambiguate.ambiguate

emitKn :: Emitter [KNF.Exp KNF.Name]
emitKn outfile = emitScheme outfile . map KnEmbed.def -- projection KN -> HOX

-- Universal Forward Translator

data UFTException = NotForward Language Language
  deriving (Show)
instance Exception UFTException

translate :: Language -> Language -> Handle -> Handle -> IO (E.Error (IO ()))
translate inLang outLang infile outfile = catch
  (case outLang of
    VS -> vsOf inLang infile <&> (emitVS outfile <$>)
    VO -> voOf inLang infile <&> (emitVO outfile <$>)
    HO -> hoOf inLang infile <&> (emitHO outfile <$>)
    KN -> knTextOf inLang infile <&> (emitKn outfile <$>))
  (\e -> case e of
    Backward -> throw (NotForward inLang outLang)
    NoTranslationTo l -> throw (NotForward inLang l))

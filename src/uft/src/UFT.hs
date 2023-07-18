{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module UFT where

import Control.Exception (Exception, catch, throw)
import Languages
import Text.Parsec (ParseError, runParser)

import Control.Monad ((>=>))
import Data.Functor ((<&>))
import Error
import GHC.IO.Handle (Handle, hGetContents, hPutStr)
import System.IO (hPutStrLn)
import Text.Parsec.String (Parser)

-- import Control.Arrow ((>>>))

import qualified Ambiguate
import qualified Asm
import qualified AsmParse
import qualified AsmUnparse
import qualified Assembler
import qualified CSUtil
import qualified ClConvert
import qualified ClScheme
import qualified CodeGen
import qualified Disambiguate
import qualified Error as E
import qualified FOClUtil
import qualified FOScheme
import qualified FOUtil
import qualified KNF
import qualified KNormalize
import qualified KnEmbed
import qualified KnProject
import qualified KnRename
import qualified ObjectCode
import qualified ObjectUnparser
import qualified ParseUtils
import qualified UnambiguousVScheme
import qualified VScheme
import qualified VSchemeParse
import qualified VSchemeUnparse

type Emitter a = Handle -> a -> IO ()
type Reader a = Handle -> IO (E.Error a)

-- (<&>) = flip fmap
-- Just 2 <&> (+1)
-- fmap (+1) Just 2
(==>) :: Reader a -> (a -> E.Error b) -> Reader b
(r ==> f) h = r h <&> (f =<<)

-- (>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
-- f >=> g     = \x -> f x >>= g

-- support for materialization

data InternalException
  = Backward
  | NoTranslationTo Language
  deriving (Show)
instance Exception InternalException

-- Reader functions

schemeOfFile :: Reader [VScheme.Def]
schemeOfFile infile = hGetContents infile <&> ParseUtils.parseAndErr VSchemeParse.parse . VSchemeParse.deComment

schemexOfFile :: Reader [UnambiguousVScheme.Def]
schemexOfFile = schemeOfFile ==> ((E.Error . Right) . map Disambiguate.disambiguate)

-- val eschemeOfFile : instream -> VScheme.def list error =
--   sourceReader EschemeParsers.defs

eschemeOfFile :: Reader [VScheme.Def]
eschemeOfFile = schemeOfFile -- we need to have eschemeParser.defs

eschemexOfFile :: Reader [UnambiguousVScheme.Def]
eschemexOfFile = eschemeOfFile ==> ((E.Error . Right) . map Disambiguate.disambiguate)

-- use just call disambiguage

vsOfFile :: Reader [Asm.Instr]
vsOfFile infile = hGetContents infile <&> ParseUtils.parseAndErr AsmParse.parse

knOfFile :: Reader [KNF.Exp String]
knOfFile = schemexOfFile ==> mapM KnProject.def -- projection HO -> KN

-- val FO_of_file : instream -> FirstOrderScheme.def list error
foOfFile :: Reader [FOScheme.Def]
foOfFile = schemexOfFile ==> mapM FOUtil.def

---- Materializer functions ----

hoxOf :: Language -> Reader [UnambiguousVScheme.Def]
hoxOf HOX = schemexOfFile
hoxOf _ = throw Backward

hoOf :: Language -> Reader [UnambiguousVScheme.Def]
hoOf HO = schemexOfFile
hoOf HOX = error "imperative features (HOX to HO)"
hoOf _ = throw Backward

esOf ES = eschemexOfFile
esOf _ = throw Backward

clOf :: Language -> Reader [ClScheme.Def]
clOf CL = clOf FO
clOf HO = hoOf HO ==> (mapM $ return . ClConvert.close)
clOf HOX = hoOf HOX ==> (mapM $ return . ClConvert.close)
clOf ES = esOf ES ==> (mapM $ return . ClConvert.close)
clOf inLang = foOf inLang ==> (mapM $ return . FOClUtil.embed)

vsOfkn :: [KNF.Exp ObjectCode.Reg] -> [Asm.Instr]
-- (CodeGen is a infallible projection), and we want to delay
-- monadic action to the last stage (==>)
vsOfkn = CodeGen.codeGen

-- flip composition (>>>) in Control.Arrow

vsOf :: Language -> Reader [Asm.Instr]
vsOf VS = vsOfFile
vsOf inLang = knRegOf inLang ==> (return . vsOfkn)

-- vsOf inLang = knOfFile ==> (knRegOfknString >=> return . vsOfkn)

voOf :: Language -> Reader [ObjectCode.Instr]
voOf VO = \_ -> return (Error $ Left "There is no reader for .vo")
voOf inLang = vsOf inLang ==> Assembler.translate

knTextOf :: Language -> Reader [KNF.Exp String]
knTextOf KN = knOfFile
knTextOf inLang = knRegOf inLang ==> knStringofknReg

-- nothing else translates into FO

foOf :: Language -> Reader [FOScheme.Def]
foOf FO = foOfFile
foOf _ = throw Backward

-- KN_reg_of that materializes a program of type ObjectCode.reg KNormalForm.exp list

knRegOfknString :: [KNF.Exp String] -> E.Error [KNF.Exp ObjectCode.Reg]
knRegOfknString = mapM (KnRename.mapx KnRename.regOfName)

knStringofknReg :: [KNF.Exp ObjectCode.Reg] -> E.Error [KNF.Exp String]
knStringofknReg = mapM (KnRename.mapx (return . KnRename.nameOfReg))

knRegOf :: Language -> Reader [KNF.Exp ObjectCode.Reg]
knRegOf KN = knOfFile ==> knRegOfknString
knRegOf inlang = clOf inlang ==> (return . map KNormalize.def)

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

emitKn :: Emitter [KNF.Exp String]
emitKn outfile = emitScheme outfile . map KnEmbed.def -- projection KN -> HOX

-- Define emitter function emitFO by composing emitScheme with embedding function FOUtil.embed. Function emitFO should look a lot like function emitKN.
emitFO :: Emitter [FOScheme.Def]
emitFO outfile = emitScheme outfile . map FOUtil.embed

emitCL :: Emitter [ClScheme.Def]
emitCL outfile = emitScheme outfile . map CSUtil.embed

-- Universal Forward Translator

data UFTException = NotForward Language Language
  deriving (Show)
instance Exception UFTException

translate :: Language -> Language -> Handle -> Handle -> IO (E.Error (IO ()))
translate inLang outLang infile outfile =
  catch
    ( case outLang of
        VS -> vsOf inLang infile <&> (emitVS outfile <$>)
        VO -> voOf inLang infile <&> (emitVO outfile <$>)
        HO -> hoOf inLang infile <&> (emitHO outfile <$>)
        KN -> knTextOf inLang infile <&> (emitKn outfile <$>)
        FO -> foOf inLang infile <&> (emitFO outfile <$>)
        CL -> clOf inLang infile <&> (emitCL outfile <$>)
        HOX -> hoxOf inLang infile <&> (emitHO outfile <$>)
        ES -> esOf inLang infile <&> (emitHO outfile <$>)
    )
    ( \case
        Backward -> throw (NotForward inLang outLang)
        NoTranslationTo l -> throw (NotForward inLang l)
    )

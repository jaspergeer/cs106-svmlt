module UFT where

import Languages
import Control.Exception (Exception, throw, catch)
import Text.Parsec (runParser, ParseError)

import GHC.IO.Handle (hGetContents', hPutStr, Handle)
import Text.Parsec.String (Parser)
import System.IO (hPutStrLn)
import Error
import Data.Functor ((<&>))
import Control.Monad ((>=>))
-- import Control.Arrow ((>>>))

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
import qualified KnRename
import qualified CodeGen
import qualified ParseUtils

type Emitter a = Handle -> a -> IO ()
type Reader a = Handle -> IO (E.Error a)

-- (<&>) = flip fmap
-- Just 2 <&> (+1)
-- fmap (+1) Just 2
(==>) :: Reader a -> (a -> E.Error b) -> Reader b
(r ==> f) h = r h <&> (f =<<)

-- (>=>)       :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
-- f >=> g     = \x -> f x >>= g

-- moved to parser util

-- parseAndErr :: Parser a -> String -> E.Error a
-- parseAndErr p input = case runParser p () "" input of
--   Left e -> E.Error $ Left (show e)
--   Right r -> E.Error $ Right r

-- support for materialization

data InternalException = Backward
                       | NoTranslationTo Language
                  deriving (Show)
instance Exception InternalException

-- Reader functions 

schemeOfFile :: Reader [VScheme.Def]
schemeOfFile infile = hGetContents' infile <&> ParseUtils.parseAndErr VSchemeParse.parse

schemexOfFile :: Reader [UnambiguousVScheme.Def]
schemexOfFile = schemeOfFile ==> ((E.Error . Right) . map Disambiguate.disambiguate)

vsOfFile :: Reader [Asm.Instr]
vsOfFile infile = hGetContents' infile <&> ParseUtils.parseAndErr AsmParse.parse

knOfFile :: Reader [KNF.Exp String]
knOfFile = schemexOfFile ==> mapM KnProject.def  -- projection HO -> KN

-- Materializer functions

hoOf :: Language -> Reader [UnambiguousVScheme.Def]
hoOf HO = schemexOfFile
hoOf HOX = error "imperative features (HOX to HO)"
hoOf _ = throw Backward

-- val VS_of_KN : ObjectCode.reg KNormalForm.exp list ->
--                AssemblyCode.instr list
--   = ... fill in with a composition of functions ...

vsOfkn :: [KNF.Exp ObjectCode.Reg] -> [Asm.Instr]
-- (CodeGen is a infallible projection), and we want to delay
-- monadic action to the last stage (==>)
vsOfkn =  concatMap CodeGen.forEffect
-- flip composition (>>>) in Control.Arrow


vsOf :: Language -> Reader [Asm.Instr]
vsOf VS = vsOfFile
vsOf KN = knOfFile ==> (knRegOfknString >=> return . vsOfkn)

--  \f g x -> x >>= f >>= g
-- RHS I want Reader [Asm.Intr]
-- I get this from 
-- knOfFile :: Reader [KNF.Exp String]
-- knRegOfknString :: [KNF.Exp String] -> E.Error [KNF.Exp ObjectCode.Reg]
-- vsOfkn :: [KNF.Exp ObjectCode.Reg] -> E.Error [Asm.Instr]

vsOf _ = throw (NoTranslationTo VS)

voOf :: Language -> Reader [ObjectCode.Instr]
voOf VO     =  \_ -> return (Error $ Left "There is no reader for .vo")
voOf inLang =  vsOf inLang ==> Assembler.translate

knTextOf :: Language -> Reader [KNF.Exp String]
knTextOf KN = knOfFile
knTextOf _ = error "bad :("

-- KN_reg_of that materializes a program of type 
-- ObjectCode.reg KNormalForm.exp list

-- Because function KN_of_file produces a list of expressions, 
-- but KNRename.mapx KNRename.regOfName renames one expression, 
-- you will want to use Error.mapList. 
-- The easiest way to get this code right is to define a helper 
-- function that does the renaming. 
-- The benefit of defining a helper function is that you can 
-- give it an explicit type signature, 
-- which will help the compiler tell you what you need to do:

-- val KN_reg_of_KN_string :
--       string KNormalForm.exp list -> 
--       ObjectCode.reg KNormalForm.exp list error
--   =  ... fill in with a composition of functions ...

knRegOfknString :: [KNF.Exp String] -> E.Error [KNF.Exp ObjectCode.Reg]
knRegOfknString = mapM (KnRename.mapx KnRename.regOfName)

knRegOf :: Language -> Reader [KNF.Exp ObjectCode.Reg]
knRegOf KN = knOfFile ==> knRegOfknString

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

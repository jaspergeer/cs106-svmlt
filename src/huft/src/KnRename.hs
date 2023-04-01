module KnRename where

-- In K Normal Form, convert string names to register names

import qualified AsmParse
import qualified Error as E
import qualified KNF as K
import qualified ParseUtils as P

regOfName :: String -> E.Error Int
regOfName ('$':s) = P.parseAndErr AsmParse.reg ('$':s)
regOfName s = P.parseAndErr AsmParse.reg ('$':s)

nameOfReg :: Int -> String
nameOfReg i = "$r" ++ show i

-- The function requires the tedious copying of program 
-- structure that you find in function Disambiguate.disambiguate.

--  The function requires the management of projection failure 
--  that you find in function ProjectKN.def.

mapx :: (a -> E.Error b) -> (K.Exp a -> E.Error (K.Exp b))
mapx f e =
  let mx = mapx f in
  case e of
    K.Name n -> K.Name <$> f n
    K.If n e1 e2 -> K.If <$> f n <*> mx e1 <*> mx e2
    K.Let n e1 e2 -> K.Let <$> f n <*> mx e1 <*> mx e2
    K.Seq e1 e2 -> K.Seq <$> mx e1 <*> mx e2
    K.Assign n e1 -> K.Assign <$> f n <*> mx e1
    K.While n e1 e2 -> K.While <$> f n <*> mx e1 <*> mx e2
    K.FunCode ns e -> K.FunCode <$> mapM f ns <*> mx e
    K.FunCall n ns -> K.FunCall <$> f n <*> mapM f ns
    K.VMOP op ns -> K.VMOP op <$> mapM f ns
    K.VMOPGLO op ns lit -> K.VMOPGLO op <$> mapM f ns <*> pure lit
    K.Literal lit -> return (K.Literal lit)

module KnRename where

import qualified AsmParse
import qualified UFT
import qualified Error as E
import qualified KNF as K

regOfName = UFT.parseAndErr AsmParse.reg

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
    
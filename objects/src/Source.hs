module Source where

import Parser
import Declare
import Target
import Data.List

translateArgM :: Method -> Var -> SigmaTerm ->Method
translateArgM (Method x  a) y s = 
  if x == y then Method x  a else Method x  (translateArg a y s )

translateArg :: SigmaTerm -> Var -> SigmaTerm -> SigmaTerm
translateArg (SigmaVar v) a s = if v == a then s else SigmaVar v
translateArg (Object l) y s = Object $ map (\(li,mi) -> (li, translateArgM mi y s)) l
translateArg (Update a l m) y s = Update (translateArg a y s) l (translateArgM m y s)
translateArg (Let x a b) y s= Let x (translateArg a y s) (translateArg b y s)
translateArg (Call a l) y s= Call (translateArg a y s) l
translateArg (Binary op a b) y s = Binary op (translateArg a y s) (translateArg b y s)
translateArg (Lit a) _ _ = Lit a
translateArg (Clone a) y s= Clone (translateArg a y s)

translateEM :: SMethod -> TClass -> Method
translateEM (SMethod v a) tc = Method v  (translate a tc)

type TClass = [(Var, Exp)]

translate :: Exp -> TClass -> SigmaTerm
translate (SVar x) tc = SigmaVar x
translate (Lam x b) tc = Object [(Label "arg", Method x  (Call (SigmaVar x) (Label "arg"))), (Label "val", Method x  (translateArg (translate b tc) x (Call (SigmaVar x) (Label "arg"))))]
-- translate (Apply b a) = Call (Update (translate b) (Label "arg") (Method (Var "_") (translate a))) (Label "val")
translate (SObject ms) tc = Object (map (\(li,mi) -> (li, translateEM mi tc)) ms)
translate (SCall e l) tc = Call (translate e tc) l
translate (SUpdate e l m) tc = Update (translate e tc) l (translateEM m tc)
translate (SLet x a b) tc = case a of 
  Class _ _ -> translate b ((x,a):tc)
  _ -> Let x (translate a tc) (translate b tc)
translate (SBin op a b) tc = Binary op (translate a tc) (translate b tc)
translate (SLit a) tc = Lit a
translate (Apply b a) tc = Let (Var "f") (Clone (translate b tc)) (Let (Var "y") (translate a tc) (Call (Update (SigmaVar (Var "f")) (Label "arg") (Method (Var "_") (SigmaVar (Var "y")))) (Label "val")))
translate (SClone a) tc = Clone (translate a tc)
-- translate (Class a b) tc = translate (classGen (Class a b) tc) tc
translate (SNew a) tc = Call (translate (classGen a tc) tc) (Label "new")
-- translate (SNew a) tc = error (show tc)

collectLabels :: [(Label, SMethod)] -> [Label]
collectLabels ms = map fst ms

classGenNew :: [Label] ->  SMethod
classGenNew [] = SMethod (Var "z") (SObject [])
classGenNew (x:xs) = case classGenNew xs of
    SMethod v (SObject ms) -> let r = (x, SMethod (Var "x") (Apply (SCall (SVar (Var "z"))  x) (SVar (Var "x")))) in SMethod v (SObject (r:ms))

classMerge :: [(Label, SMethod)] -> [(Label, SMethod)] -> [(Label, SMethod)]
classMerge a [] = a
classMerge a ((x,y):xs) = case lookup x a of
    Just _ -> classMerge a xs
    Nothing -> classMerge ((x, SMethod (Var "_") (SCall (SVar (Var "super")) x)):a) xs

classConvert :: [(Label, SMethod)] -> [(Label, SMethod)]
classConvert [] = []
classConvert ((l, SMethod x m):xs) = (l, SMethod (Var "_") (Lam x m)):(classConvert xs) 

classGen :: Exp -> TClass -> Exp
classGen c tc = case c of
  SVar x -> case lookup x tc of
    Just s -> classGen s tc
    _ -> error ((show x) ++ " not defined" ) 
  Class ms Top -> let labels = collectLabels ms in
      let new = (Label "new", classGenNew labels) in (SLet (Var "super") (SObject []) (SObject (new:(classConvert ms))))
  Class ms fa -> let SLet _ _ (SObject (_:father)) = classGen fa tc in 
      let newMethods = classMerge (classConvert ms) father in 
          let labels = collectLabels newMethods in
              let new = (Label "new", classGenNew labels) in (SLet (Var "super") (SObject father) (SObject (new:newMethods)))
  _ -> error "Not a Class"


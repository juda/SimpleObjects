module Target where

import Parser
import Declare
import Data.List
import Data.Maybe

{-

The type Mem is a model of memory: it is used to store objects in memory.
For this language we have to have memory, just as in the interpreter
with mutable state, because objects are mutable. That is, the method
update operation can actually modify methods stored in objects.

Therefore the memory stores all the objects that are allocated in the program.
-}

type Object = [(Label, MethodClosure)]

type Mem = [Object]

{- The replace operation, updates a method for an object in memory -}

replace :: Int -> Label -> MethodClosure -> Mem -> Mem
replace i l closure mem =
    let (before, o : after) = splitAt i mem in
    before ++ [[if (l == n) then (n, closure) else (n,c) | (n,c) <- o]] ++ after

{- Evaluation takes the following arguments:

SigmaTerm: The expression to be evaluated
Env: The current environment
Mem: The current memory

and returns the following:

Value: The value that is computed
Mem: The updated memory (in case method update operations have been performed)

-}

evaluateS :: SigmaTerm -> Env -> Mem -> Maybe (Value, Mem)
evaluateS (SigmaVar v) e mem = case lookup v e of
  Just a -> Just (a, mem)
  Nothing -> error ("Variable " ++ show v ++ " is undefined!")
evaluateS (Object o) e mem =
  let mc = map (\(l,Method v t) -> (l,Closure e v t)) o
  in Just (ObjRef (length mem), mem ++ [mc])
evaluateS (Call a l) e mem = 
    case evaluateS a e mem of
      Just (ObjRef i,mem') ->
        let ms = mem' !! i
        in case lookup l ms of
              Just (Closure env v m) -> evaluateS m ((v,ObjRef i) : env) mem'
              _ -> error ("Method not found: The method " ++ show l
                          ++ " was not found in:\n" ++ show ms) 
      _ -> error ("Type error: The expression:\n "  ++
                     show a ++ "\n does not evaluate to an object!")
evaluateS (Let x a b) e mem = 
  case evaluateS a e mem of
    Just (a',mem') -> evaluateS b ((x, a'):e) mem'
    Nothing -> Nothing
evaluateS (Clone a) e mem = 
  case evaluateS a e mem of
      Just (ObjRef i,mem') ->
        let ms = mem' !! i in Just (ObjRef (length mem'), mem' ++ [ms])
      _ -> error ((show a) ++ " should be an object")
evaluateS (Lit a) e  mem    = Just (VInt a, mem)
evaluateS (Boolean a) e mem = Just (VBool a, mem)
evaluateS (Binary Add a b) e mem = case evaluateS a e mem of
  Just (VInt a1, mem') -> case evaluateS b e mem' of
    Just (VInt b1, mem'') -> Just (VInt (a1+b1), mem'')
    _ -> error ("Type error: The expression:\n" ++
               show b ++ "\n does not evaluate to an integer!")
  _ -> error ("Type error: The expression:\n" ++
               show a ++ "\n does not evaluate to an integer!")                                   
evaluateS (Update a l (Method v m)) e mem =
  case evaluateS a e mem of
    Just (ObjRef i, mem') ->
      Just (ObjRef i, replace i l (Closure e v m) mem')
evaluateS (Binary op a b) e mem = error "TODO"
evaluateS (Unary op a) e mem = error "TODO"
evaluateS (If a b c) e mem = error "TODO"

execute :: SigmaTerm -> SigmaTerm
execute e = case evaluateS e [] [] of
  Just (ObjRef i, ms) ->  revert (ms !! i)
  Just (VInt i, ms) -> Lit i
  _ -> error "Answer is not a value"

revert :: Object -> SigmaTerm
revert [] = Object []
revert (x:xs) = let (l, cl) = x in
  case cl of
    Closure _ y s -> let mt = Method y s in
      case revert xs of
        Object ms -> Object ((l, mt):ms)
        _ -> error "Revert function fails"

-- | Tests for Target
--
-- >>> o1 = Object [(Label "l", Method (Var "x")  (Call (SigmaVar (Var "x")) (Label "l")))]
-- >>> o2 = Object [(Label "l", Method (Var "x")  (SigmaVar (Var "x")))]
-- >>> o3 = Object [(Label "l", Method (Var "y")  (Update (SigmaVar (Var "y")) (Label "l") (Method (Var "x") (SigmaVar (Var "x")))))]
-- >>> p1 = Call o1 (Label "l")
-- >>> p2 = Call o2 (Label "l")
-- >>> p3 = Call o3 (Label "l")
-- >>> a2 = execute p2
-- >>> a3 = execute p3
-- >>> a2 == o2
-- True
-- >>> a3 == o2
-- True
-- >>> vtrue = Object [(Label "if", Method (Var "x")  (Call (SigmaVar (Var "x")) (Label "then"))), (Label "then", Method (Var "x")  (Call (SigmaVar (Var "x")) (Label "then"))), (Label "else", Method (Var "x")  (Call (SigmaVar (Var "x")) (Label "else")))]
-- >>> vfalse = Object [(Label "if", Method (Var "x")  (Call (SigmaVar (Var "x")) (Label "else"))), (Label "then", Method (Var "x")  (Call (SigmaVar (Var "x")) (Label "then"))), (Label "else", Method (Var "x")  (Call (SigmaVar (Var "x")) (Label "else")))]
-- >>> cond b c d = Call (Update (Update b (Label "then") (Method (Var "_") c)) (Label "else") (Method (Var "_") d)) (Label "if")
-- >>> if1 = cond vtrue vfalse vtrue
-- >>> (execute if1) == vfalse
-- True
-- >>> if2 = cond vfalse vfalse vtrue
-- >>> (execute if2) == vtrue
-- True

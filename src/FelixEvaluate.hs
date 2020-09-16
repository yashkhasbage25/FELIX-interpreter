module FelixEvaluate where

import Prelude
import Control.Exception (throw, throwIO)
import Control.Applicative ((<$>))
import Control.Monad (when)

import FelixError
import FelixSyntax
import FelixEnvironment

-- | "evalExpr" function evaluates an expression for each type of expression defined in Syntax.hs
evalExpr :: Environment -> Expression -> IO Value
evalExpr _ (Const (ConstBool bool)) = return (BooleanValue bool)
evalExpr _ (Const (ConstStr str)) = return (StringValue str)
evalExpr _ (Const (ConstDbl num)) = return (DoubleValue num)

-- | value return will be of type closure conataining arguments and codebody as its instances
evalExpr env (Funct identifiers codebody) = return (Closure env identifiers codebody)

{-|
    If the expression is operator expression then first the expressions are evaluated
    and then evalOp is called which evaluates the values as per operator
-}
evalExpr env (Op op exp1 exp2) = do
    leftValue <- evalExpr env exp1
    rightValue <- evalExpr env exp2
    return (evalOp op leftValue rightValue)

{-|
    Apply will be parsed when we call a function and first we evaluate a expression (as function id)
    and it will recurse and go for 'evalExpr functionId' whiich eventually returns value ("closure" instance)
    then we can switch to Closure and when we call a new function we are creating new environment
    containing maping of argument and their value .
-}
evalExpr env (Apply expr arguments) = do
    valreturn <- evalExpr env expr
    case valreturn of
        Closure curentenv identifiers codebody -> do
            when (length identifiers /= length arguments) $
                throwIO $ FelixError "the number of arguments are wrong"
            values <- mapM (evalExpr env) arguments
            newclsenv' <- newConsEnvFun curentenv (zip identifiers values)
            output <- run codebody newclsenv'
            maybe (return Undef) return output
        PredefFunc funct ->mapM (evalExpr env) arguments >>= funct
        _ -> throwIO $FelixError "not a function"
evalExpr env (Id id) = envLookup env id

-- | "evalExpr" function evaluates an Operator expression for all the operators.
evalOp :: OP -> Value -> Value -> Value
evalOp op leftValue rightValue = evalOpp op leftValue rightValue
  where
    evalOpp ADD (DoubleValue n1) (DoubleValue n2) = DoubleValue (n1 + n2)
    evalOpp SUB (DoubleValue n1) (DoubleValue n2) = DoubleValue (n1 - n2)
    evalOpp MUL (DoubleValue n1) (DoubleValue n2) = DoubleValue (n1 * n2)
    evalOpp DIV (DoubleValue n1) (DoubleValue n2) = DoubleValue (n1 / n2)

    evalOpp EQL (DoubleValue n1) (DoubleValue n2) = BooleanValue (n1 == n2)
    evalOpp EQL (StringValue str1) (StringValue str2) = BooleanValue (str1 == str2)
    evalOpp EQL (BooleanValue b1) (BooleanValue b2) = BooleanValue (b1 == b2)
    evalOpp NEQ v1 v2 = case evalOpp EQL v1 v2 of
        BooleanValue b -> BooleanValue (not b)
        _ -> throw $ FelixError "must be of boolean type"
    evalOpp GTT (DoubleValue n1) (DoubleValue n2) = BooleanValue (n1 > n2)
    evalOpp GE (DoubleValue n1) (DoubleValue n2) = BooleanValue (n1 >= n2)
    evalOpp LTT (DoubleValue n1) (DoubleValue n2) = BooleanValue (n1 < n2)
    evalOpp LE (DoubleValue n1) (DoubleValue n2) = BooleanValue (n1 <= n2)

    evalOpp AND (BooleanValue b1) (BooleanValue b2) = BooleanValue (b1 && b2)
    evalOpp OR (BooleanValue b1) (BooleanValue b2) = BooleanValue (b1 || b2)
    evalOpp _ _ _ = throw $ FelixError $ concat ["Incorrect arguments to evalOp",show leftValue," ",show op," ",show rightValue]


-- | "evalExpr" function evaluates Statement for each type of statement defined in Syntax.hs

evalStmt :: Environment -> Statement -> IO (Maybe Value)
{-|
    Evaluates Variable Decleration Statment. First evaluates expression and then
    declares this ident,value pair in environment
-}
evalStmt env (Var ident expr) = do
    value <- evalExpr env expr
    envDeclare env ident value
    return Nothing

{-|
    Evaluates Assignment Statement , First evaluates expression and then assigns it
-}
evalStmt env (Assign ident expr) = do
    value <- evalExpr env expr
    envAssign env ident value
    return Nothing
{-|
    Evaluates Condition statement by first evaluating the condition and executing Trueblock and
    false block according to condition
-}
evalStmt env (If expr trueBlock falseBlock) = do
    condition <- evalExpr env expr
    case condition of
        BooleanValue True -> newConsEnv env >>= run trueBlock
        BooleanValue False -> newConsEnv env >>= run falseBlock

{-|
    While loop keeps on evaluating condtition untill condition is evaluated to False
    after executing block run function return Nothing as a result maybe function chooses default
    condition and keeps looping
-}
evalStmt env (While expr block) = do
    condition <- evalExpr env expr
    case condition of
        BooleanValue True -> do
            result <- run block env
            maybe  (evalStmt env (While expr block)) (return . Just) result

        BooleanValue False -> return Nothing
        _ -> throwIO $ FelixError "Condition of while statement must be of boolean type"

evalStmt env (Expr expr) = evalExpr env expr >> return Nothing -- always return Nothing
evalStmt env (Return expr) = Just <$> evalExpr env expr


-- | "run" function evaluates multiple statements
run :: Statements -> Environment -> IO (Maybe Value)
run [] _ = return Nothing
run (stmt : stmts) env = do
    result <- evalStmt env stmt
    maybe (run stmts env) (return . Just) result

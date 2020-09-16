module TestEvaluate
(testsOpEval)
where

import Test.HUnit
import Data.IORef
import FelixEvaluate
import FelixSyntax
import FelixEnvironment
import FelixEvaluate
import FelixPredefined

testsOpEval = TestList [TestLabel "Add DoubleVal" addOpDoubleVal,
    TestLabel "Sub DoubleVal" subOpDoubleVal,
    TestLabel "Mult DoubleVal" mulOpDoubleVal,
    TestLabel "Div DoubleVal" divOpDoubleVal,
    TestLabel "Greater than DoubleVal" greaterOpDoubleVal,
    TestLabel "Less than DoubleVal" lessOpDoubleVal,
    TestLabel "And Boolval" andOpBooleanVal1,
    TestLabel "And Boolval" andOpBooleanVal2,
    TestLabel "OR Boolval" orOpBooleanVal1,
    TestLabel "OR Boolval" orOpBooleanVal2]


addOpDoubleVal = test ["test1"~: "evalOp 1+2" ~: (DoubleValue 3) ~=? (evalOp ADD (DoubleValue 1) (DoubleValue 2))]
subOpDoubleVal = test ["test1"~: "evalOp 10-2" ~: (DoubleValue 8) ~=? (evalOp SUB (DoubleValue 10) (DoubleValue 2))]
mulOpDoubleVal = test ["test3"~: "evalOp 5*2" ~: (DoubleValue 10) ~=? (evalOp MUL (DoubleValue 5) (DoubleValue 2))]
divOpDoubleVal = test ["test4"~: "evalOp 10/2" ~: (DoubleValue 5) ~=? (evalOp DIV (DoubleValue 10) (DoubleValue 2))]
greaterOpDoubleVal = test ["test5"~: "evalOp 10>2" ~: (BooleanValue True) ~=? (evalOp GTT (DoubleValue 10) (DoubleValue 2))]
lessOpDoubleVal = test ["test6"~: "evalOp 10<2" ~: (BooleanValue False) ~=? (evalOp LTT (DoubleValue 10) (DoubleValue 2))]
andOpBooleanVal1 = test ["test 7"~: "evalOp True and True"~: (BooleanValue True)~=? (evalOp AND (BooleanValue True) (BooleanValue True)) ]
andOpBooleanVal2 = test ["test 8"~: "evalOp True and False"~: (BooleanValue False)~=? (evalOp AND (BooleanValue False) (BooleanValue True)) ]
orOpBooleanVal1 = test ["test 9"~: "evalOp True and False"~: (BooleanValue True)~=? (evalOp OR (BooleanValue False) (BooleanValue True)) ]
orOpBooleanVal2 = test ["test 10"~: "evalOp False and False"~: (BooleanValue False)~=? (evalOp OR (BooleanValue False) (BooleanValue False)) ]

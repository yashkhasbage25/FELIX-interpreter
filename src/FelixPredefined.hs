module FelixPredefined where

import Prelude hiding (not, length, print)
import qualified Prelude as P
import Control.Exception (throwIO)
import Data.IORef
import Data.List (intercalate)
import qualified Data.Map as M

import FelixError
import FelixSyntax
import FelixEnvironment

initialEnv :: IO Environment
initialEnv = newIORef (Top (M.fromList predefined))

predefined :: [(ID, Value)]
predefined = map (\(s, f) -> (GetID s, PredefFunc f))
    [ ("not", not)
    , ("number_to_string", numtostr)
    , ("boolean_to_string", booltostr)
    , ("number?", isnum)
    , ("string?", isstr)
    , ("boolean?", isbool)
    , ("length", len)
    , ("print", print)
    , ("println", println)
    ]
  where
    predefError name args = throwIO $ FelixError $ concat
        [ "invalid arguments: "
        , name
        , "(" ++ intercalate "," (map show args) ++ ")"
        ]
    bool = return . BooleanValue
    str = return . StringValue
    num = return . DoubleValue

    not [BooleanValue b] = bool (P.not b)
    not args = predefError "not" args

    numtostr [DoubleValue n] = str (show n)
    numtostr args = predefError "number_to_string" args

    booltostr [BooleanValue b] = str (show (BooleanValue b))
    booltostr args = predefError "boolean_to_string" args

    isnum [DoubleValue _] = bool True
    isnum [_] = bool False
    isnum args = predefError "number?" args

    isbool [BooleanValue _] = bool True
    isbool [_] = bool False
    isbool args = predefError "boolean?" args

    isstr [StringValue _] = bool True
    isstr [_] = bool False
    isstr args = predefError "string?" args

    len [StringValue s] = num . fromInteger . toInteger . P.length $ s
    len args = predefError "length" args

    print args = do
        putStr $ intercalate "" $ map show args
        return Undef

    println args = do
        putStrLn $ intercalate "" $ map show args
        return Undef

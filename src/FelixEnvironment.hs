module FelixEnvironment where

import Prelude
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Control.Exception

import FelixSyntax
import FelixError

-- | Constructors for various kinds of values
data Value =  DoubleValue Double  -- ^ Double value constructor
             | BooleanValue Bool  -- ^ Bool value constructor
             | StringValue String -- ^ string value constructor
             | Closure Environment [ID] [Statement] -- ^ closure value constructor
             | PredefFunc ([Value] -> IO Value) -- ^ Predefined function
             | Undef                -- ^ undef value constructor

-- | making Value type instance of show.
instance Show Value where
    show (DoubleValue number) = show number
    show (BooleanValue True) = "True"
    show (BooleanValue False) = "False"
    show (StringValue string) = string
    show (Closure _ _ _) = "<closure>"
    show Undef = "<undefined>"

-- | instances of Value for testing purposes
instance Eq Value where
    DoubleValue l == DoubleValue r = if ((abs $ l - r) <  1e-3) then True else False
    BooleanValue l == BooleanValue r = l == r

-- | Mutable EnvList in 'IO' Monad
type Environment = IORef EnvList

-- | constructors for envlist.
data EnvList = Top (Map ID Value)  -- ^ Top constructor storing map  of id and value
             | Cons (Map ID Value) Environment  -- ^ storing map of id and value alongwith mutable Envlist.


-- | "envLookup" function look for value of id inside Environment
envLookup :: Environment -> ID -> IO Value
envLookup env id = do
    envlist <- readIORef env
    case envlist of
        Top first -> maybe (return Undef) return $ M.lookup id first
        Cons first nxt -> maybe (envLookup nxt id) return $ M.lookup id first

-- | "envDeclare" function insert (id,value) in mapping present in Environment.
envDeclare :: Environment -> ID -> Value -> IO ()
envDeclare env id value = do
    envlist <- readIORef env
    case envlist of
        Top first -> writeIORef env (Top (M.insert id value first))
        Cons first nxt -> writeIORef env (Cons (M.insert id value first) nxt)

{-|
    "envAssign" function assign value to id present in environment mapping.
    If that Id is not present in the Environment then it stores error message in
    Felixerror string.
-}
envAssign :: Environment -> ID -> Value -> IO ()
envAssign env id value = do
    envlist <- readIORef env
    case envlist of
        Top first | M.member id first -> writeIORef env (Top (M.insert id value first))
        Top _ -> throwIO $ FelixError $ "non declared identifier '" ++ tokenName id ++ "'"
        Cons first nxt | M.member id first -> writeIORef env (Cons (M.insert id value first) nxt)
        Cons _ nxt -> envAssign nxt id value

-- | "newConsEnv" creates new empty  cons environment.
newConsEnv :: Environment -> IO Environment
newConsEnv = newIORef . Cons M.empty

-- | "newConsEnvFun" creates new cons environment and add given mapping pairs.
newConsEnvFun :: Environment -> [(ID, Value)] -> IO Environment
newConsEnvFun env mapings = newIORef (Cons (M.fromList mapings) env)

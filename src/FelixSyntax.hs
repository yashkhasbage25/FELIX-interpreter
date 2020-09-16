module FelixSyntax where

type Statements = [Statement] -- ^ Block of Statements

-- | Value Constructors for Statement
data Statement = Var ID Expression  -- ^Varaible Decleration Statement
               | Assign ID Expression -- ^ Assignment Statement
               | If Expression [Statement] [Statement] -- ^ If Conditional Statement
               | Expr Expression
               | Return Expression
               | While Expression [Statement]
               deriving (Show)

-- | Value Constructors for Expression
data Expression = Const Constant -- ^ Constant Expression
          | Op OP Expression Expression -- ^ Expression with Operators
          | Funct [ID][Statement] -- ^ Expression while initialising function
          | Apply Expression [Expression] -- ^ Expression while calling function
          | Id ID -- ^  Expression as identifier
          deriving (Show)

-- | Value Constructors for Constant
data Constant = ConstDbl Double  -- ^ Double Constant
           | ConstBool Bool      -- ^ Boolean Constant
           | ConstStr String     -- ^ String Constant
           deriving (Show)

-- | Value Constructor of ID
data ID = GetID { tokenName :: String } deriving (Eq, Ord, Show)


-- | Value Constructors for Operators
data OP = ADD |  -- ^ Arithmetic Operator
        SUB | -- ^ Arithmetic Operator
        MUL | -- ^ Arithmetic Operator
        DIV | -- ^ Arithmetic Operator
        EQL | -- ^ Relationl Operator
        NEQ | -- ^ Relationl Operator
        LTT | -- ^ Relationl Operator
        LE  | -- ^ Relationl Operator
        GTT | -- ^ Relationl Operator
        GE  | -- ^ Relationl Operator
        AND | -- ^ Logical Operator
        OR    -- ^ Logical Operator

-- | Making OP datatype instance of Show
instance Show OP where
    show ADD = "+"
    show SUB = "-"
    show MUL = "*"
    show DIV = "/"
    show EQL = "=="
    show NEQ = "!="
    show LTT = "<"
    show LE = "<="
    show GTT = ">"
    show GE = ">="
    show AND = "&&"
    show OR = "||"

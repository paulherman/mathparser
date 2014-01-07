type Value = Float
type Variable = String
type Function = String
type Operator = String
data Associativity = Left | Right deriving (Eq, Ord)
type OpEnvironment = [(Operator, Value -> Value -> Value, Associativity)]
type VarEnvironment = [(Variable, Value)]
type FunEnvironment = [(Function, [Value] -> Value)]
data Expression = Val Value | Var Variable | Fun Function [Expression] | Op Operator Expression Expression

instance Show Expression where
	show (Val x) = show x
	show (Var v) = v
	show (Op op x y) = "(" ++ show x ++ op ++ show y ++ ")"
	show (Fun f es) = f ++ "(" ++ showFunArgs es ++ ")"

showFunArgs (x : xs : xss) = show x ++ ", " ++ showFunArgs (xs : xss)
showFunArgs (x : xs) = show x

evaluate :: VarEnvironment -> OpEnvironment -> FunEnvironment -> Expression -> Value
evaluate varEnv opEnv funEnv (Val x) = x
evaluate varEnv opEnv funEnv (Var v) = varEnvGet varEnv v
evaluate varEnv opEnv funEnv (Fun f ls) = ((funEnvGet funEnv f) . (map evalEnv)) ls
	where evalEnv = evaluate varEnv opEnv funEnv
evaluate varEnv opEnv funEnv (Op op x y) = (opEnvGet opEnv op) (evalEnv x) (evalEnv y)
	where evalEnv = evaluate varEnv opEnv funEnv

funEnvEmpty :: FunEnvironment
funEnvEmpty = []

funEnvSet :: FunEnvironment -> Function -> ([Value] -> Value) -> FunEnvironment
funEnvSet funEnv name f = (name, f) : filter (\(n, f) -> n /= name) funEnv

funEnvGet :: FunEnvironment -> Function -> ([Value] -> Value)
funEnvGet funEnv name
	| null fs = error "Function not in environment"
	| otherwise = (snd . head) fs
	where fs = filter (\(n, v) -> n == name) funEnv

opEnvEmpty :: OpEnvironment
opEnvEmpty = []

opEnvSet :: OpEnvironment -> Operator -> (Value -> Value -> Value) -> Associativity -> OpEnvironment
opEnvSet opEnv name op assoc = (name, op, assoc) : filter (\(n, v, a) -> n /= name) opEnv

opEnvGet :: OpEnvironment -> Operator -> (Value -> Value -> Value)
opEnvGet opEnv name
	| null ops = error "Operator not in environment"
	| otherwise = (getOpFun . head) ops
	where
		ops = filter (\(n, o, a) -> n == name) opEnv
		getOpFun (n, o, a) = o
opEnvGetAssoc :: OpEnvironment -> Operator -> Associativity
opEnvGetAssoc opEnv name
	| null ops = error "Operator not in environment"
	| otherwise = (getOpAssoc . head) ops
	where
		ops = filter (\(n, o, a) -> n == name) opEnv
		getOpAssoc (n, o, a) = a

varEnvEmpty :: VarEnvironment
varEnvEmpty = []

varEnvSet :: VarEnvironment -> Variable -> Value -> VarEnvironment
varEnvSet varEnv name val = (name, val) : filter (\(n, v) -> n /= name) varEnv

varEnvGet :: VarEnvironment -> Variable -> Value
varEnvGet varEnv name
	| null vars = error "Variable not in environment"
	| otherwise = (snd . head) vars
	where vars = filter (\(n, v) -> n == name) varEnv

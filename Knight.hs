module Compiler where


import Data.Char
import System.Environment
import System.IO
import GHC.Float
import GHC.Word
import GHC.Base as B
import Numeric
import Data.Set as Set
import Data.Map as Map
import Control.Monad.State.Strict as C

import qualified LLVM.Core as Core
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Expr as E
import Text.ParserCombinators.Parsec.Combinator

{-
 - 1. parsing
 - 2. semantic analysis
 - 3. code emission
 - -}

languageStyle =
	javaStyle
		{
			reservedOpNames = ["*", "/","+","-", "=",
				"<",">","<=",">=","==","!=",
				"and","or","(",")"],
			reservedNames = ["return","function","true","false","if","then","else","while","mod"]
		}

lexer :: P.TokenParser ()
lexer = P.makeTokenParser languageStyle

braces = P.braces lexer
whiteSpace = P.whiteSpace lexer
lexeme = P.lexeme lexer
symbol = P.symbol lexer
natural = P.natural lexer
parens = P.parens lexer
semi = P.semi lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
stringLiteral = P.stringLiteral lexer
commaSep = P.commaSep lexer

parse_program :: Parser [Statement]
parse_program = do
	many parse_global_statement

parse_global_statement :: Parser Statement
parse_global_statement =
	do
		(reserved "public"
		<|> reserved "private")
		parse_script_decl
	<|> do
		reserved "function"
		parse_script_fun
	<|> do
		reserved "class"
		parse_script_lass
	<|> do
		(reserved "const"
		<|> reserved "var")
		parse_script_var
	<|>
		parse_statement

-- in original it returns list that contains mix of these 3 types
parse_block =
	do
		braces
			do
				reserved "var"
				parse_local_variables
			<|> do
				reserved "function"
				parse_local_function
			<|> do
				reserved "class"
				parse_local_class
			<|>
				parse_statement

parse_statement :: Parser Statement
parse_statement =
	parse_block
	<|> parse_if
	<|> parse_while
	<|> parse_do
	<|> parse_for
	<|> parse_return
	<|> parse_expr_statement --potentially try needed

parse_class

parse_function

parse_fun_expr

parse_local_class

parse_local_function

parse_local_variables

parse_return =
	do
		reserved "return"
		return $ ReturnStatement

parse_if =
	do
		reserved "if"
		cond <- parse_expression
		reserved "then"
		block_true <- parse_block
		reserved "else"
		block_false <- parse_block
		return $ IfStatement cond block_true block_false

parse_while =
	do
		reserved "while"
		cond <- parens parse_expression
		block <- parse_block
		return $ WhileStatement cond block

parse_do =
	do
		reserved "do"
		block <- parse_block
		reserved "while"
		cond <- parens parse_expression
		return $ DoStatement cond block

parse_for = return ForStatement

parse_expression = parse_conditional --expr here with the table

-- conditional
-- or
-- and
-- not
-- equals
-- relational
-- bitor
-- bitxor
-- bitand
-- bitshift
-- cfold
-- -
-- *
-- unary

parse_primitive :: Parser Expression
parse_primitive =
	do
		reserved "true"
		return $ ConstantExpression $ BooleanConstant True
	<|> do
		reserved "false"
		return $ ConstantExpression $ BooleanConstant False
	<|> do
		reserved "null"
		return $ ConstantExpression NullConstant
	<|> do
		s <- stringLiteral
		return $ ConstantExpression $ StringConstant s
	<|> do
		n <- natural
		return $ ConstantExpression $ IntegerConstant n
	<|> do
		reserved "function"
		parse_fun_expr
	<?>
		"primitive"
		
parse_prefix :: Parser Expression
parse_prefix =
	do
		reserved "{"
		parse_expression
		reserved "}"
	<|> do
		reserved "new"
		return Expression
	<|> do
		parse_primitive

parse_primary =
!!	TODO
		

parse_script_decl =
	do
		reserved "function"
		parse_script_fun
	<|> do
		reserved "const"
		parse_script_var
	<|> do
		reserved "var"
		parse_script_var

parse_script_class = identifier

parse_script_fun = identifier

parse_script_var = identifier

data Constant = NullConstant | IntegerConstant Integer | BooleanConstant Bool | FloatConstant Float | StringConstant String deriving Eq

data Expression = ConstantExpression Constant | UnaryExpression | BinaryExpression | AssignmentExpression Expression Expression

data Statement  = Statement

{-
runTest p input = parseTest (do {
			whiteSpace
			; x <- p
			; eof
			; return x
			}) input

data Program = Program [Function] deriving Show

parse_program = do
	whiteSpace
	f <- many parse_function
	eof
	return (Program f)

data Function = Function Id [Id] [Instruction] deriving Show

parse_function = do
	reserved "function"
	id <- identifier
	args <- parens (do
		commaSep (parse_id))
	braces (do
		ins <- many parse_instruction
		return (Function id args ins))
	<?> "function"

parse_id = do
	id <- identifier
	return id
	<?> "identifier"

parse_instruction = do
	parse_if_instruction
	<|> parse_while_instruction
	<|> parse_expression_instruction
	<|> parse_return_instruction
	<?> "instruction"

data Instruction = ExpressionInstruction Expression | ReturnInstruction (Maybe Expression) | IfInstruction Expression [Instruction] [Instruction] | WhileInstruction Expression [Instruction] deriving Show

parse_if_instruction = do
	reserved "if"
	cond <- parens expr
	reserved "then"
	then_case <- braces (many parse_instruction)
	reserved "else"
	else_case <- braces (many parse_instruction)
	return (IfInstruction cond then_case else_case)
	<?> "if instruction"

parse_while_instruction = do
	reserved "while"
	cond <- parens (expr)
	body <- braces (many parse_instruction)
	return (WhileInstruction cond body)
	<?> "while instruction"

parse_expression_instruction = do
	e <- expr
	semi
	return (ExpressionInstruction e)

parse_return_instruction =
	(do { reserved "return" ; ((do { semi; return (ReturnInstruction Nothing) }) <|> (do {e <- expr; semi; return (ReturnInstruction (Just e)) })) <?> "return" })

expr :: Parser Expression
expr = E.buildExpressionParser table factor
	<?> "expression"

opPrefix s f = E.Prefix (do { reservedOp s ; return f} <?> "prefix operator")

table = [[opPrefix "-" create_neg_expr]
	,[op "*" create_mul_expr E.AssocLeft, op "/" create_div_expr E.AssocLeft, keywordOp "mod" create_mod_expr E.AssocLeft ]
	,[op "+" create_add_expr E.AssocLeft, op "-" create_subtract_expr E.AssocLeft]
	,[op "<" create_l_expr E.AssocLeft, op "<=" create_le_expr E.AssocLeft, op ">" create_g_expr E.AssocLeft, op ">=" create_ge_expr E.AssocLeft
		,op "==" create_equal_expr E.AssocLeft, op "!=" create_ne_expr E.AssocLeft]
	,[keywordOp "and" create_and_expr E.AssocLeft, keywordOp "or" create_or_expr E.AssocLeft]
	,[opAssign "=" create_assign_expr E.AssocRight]]
	where
		op s f assoc = E.Infix (do { reservedOp s ; return f} <?> "operator") assoc
		opAssign s f assoc = E.Infix (do { reservedOp s ; return f} <?> "assignment") assoc
		keywordOp s f assoc = E.Infix (do { reserved s ; return f} <?> "operator") assoc

factor = parens expr
	<|> parse_constant
	<|> try parse_function_call
	<|> parse_variable
	<?> "simple expression"

-- FIXME: float constant i niejednoznacznosc
parse_constant = do
	reserved "true"
	return (ConstantExpression (BooleanConstant True))
	<|> do
	reserved "false"
	return (ConstantExpression (BooleanConstant False))
	<|> do
	i <- natural
	return (ConstantExpression (IntegerConstant i))
	<|> do
	s <- stringLiteral
	return (ConstantExpression (StringConstant s))
	<?> "constant"

type Id = String

data Variable = Variable Id deriving (Eq, Show)

parse_variable :: Parser Expression
parse_variable = do
	id <- identifier
	return (VariableExpression id)
	<?> "Variable"

parse_variable_declaration :: Parser Variable
parse_variable_declaration = do
	reserved "var"
	id <- identifier
	semi
	return (Variable id)
	<?> "Variable"

parse_function_call :: Parser Expression
parse_function_call = do
	id <- identifier
	expr_list <- parens (commaSep expr)
	return (FunctionCall id expr_list)

data Operator = UnaryNegate | BinaryAdd | BinarySubtract | BinaryMul | BinaryDiv | BinaryLess | BinaryLessEqual | BinaryGreater | BinaryGreaterEqual | BinaryEqual | BinaryNotEqual | BinaryOr | BinaryAnd | BinaryModulo deriving Eq

instance Show Operator where
	show (UnaryNegate) = "-"
	show (BinaryAdd) = "+"
	show (BinarySubtract) = "-"
	show (BinaryMul) = "*"
	show (BinaryDiv) = "/"

data Constant = IntegerConstant Integer | BooleanConstant Bool | FloatConstant Float | StringConstant String deriving Eq

instance Show Constant where
	show (IntegerConstant i) = show i
	show (BooleanConstant b) = show b
	show (FloatConstant f) = show f
	show (StringConstant s) = show s

data Expression = ConstantExpression Constant | UnaryExpression Expression Operator | BinaryExpression Expression Expression Operator | VariableExpression Id | Assignment Expression Expression | FunctionCall Id [Expression] | ArrayAccess Expression Expression | LocalExpression Id | ArgumentExpression Id | AnalysedFunctionCall ClrType Id [Expression] deriving (Eq, Show)

create_add_expr a b = BinaryExpression a b BinaryAdd
create_subtract_expr a b = BinaryExpression a b BinarySubtract
create_mul_expr a b = BinaryExpression a b BinaryMul
create_div_expr a b = BinaryExpression a b BinaryDiv
create_mod_expr a b = BinaryExpression a b BinaryModulo
create_neg_expr a = UnaryExpression a UnaryNegate
create_g_expr a b = BinaryExpression a b BinaryGreater
create_ge_expr a b = BinaryExpression a b BinaryGreaterEqual
create_l_expr a b = BinaryExpression a b BinaryLess
create_le_expr a b = BinaryExpression a b BinaryLessEqual
create_equal_expr a b = BinaryExpression a b BinaryEqual
create_ne_expr a b = BinaryExpression a b BinaryNotEqual
create_and_expr a b = BinaryExpression a b BinaryAnd
create_or_expr a b = BinaryExpression a b BinaryOr
create_assign_expr a b = Assignment a b

data AnalysisState = AnalysisState { warnings :: [String], errors :: [String] }

fresh_analysis_state = AnalysisState [] []
add_warning_helper sem_state warning = sem_state { warnings = (warnings sem_state) ++ [warning] }
add_error_helper sem_state error = sem_state { errors = (errors sem_state) ++ [error] }

add_warning warning = do
	state <- get
	put (add_warning_helper state warning)

add_error error = do
	state <- get
	put (add_error_helper state error)

display_analysis_state :: AnalysisState -> IO ()
display_analysis_state state = do
	putStrLn ("Warnings: " ++ show (length (warnings state)))
	forM_ (warnings state) putStrLn
	putStrLn ("Errors: " ++ show (length (errors state)))
	forM_ (errors state) putStrLn

analysis_state_ok state = errors state == []

-- it can be only the last instruction in the function
check_return_instruction :: [Instruction] -> Bool
check_return_instruction [] = True
check_return_instruction [(ReturnInstruction m)] = True
check_return_instruction ((ReturnInstruction m):t) = False
check_return_instruction (h:t) = check_return_instruction t

is_return_instruction (ReturnInstruction m) = True
is_return_instruction i = False

data AnalysedProgram = AnalysedProgram [AnalysedFunction]
data AnalysedFunction = AnalysedFunction { id :: Id, return_type :: ClrType, arguments :: [Id], locals :: [Variable], instructions :: [Instruction] }
data FunctionInfo = FunctionInfo Id [Id] ClrType deriving Eq

function_to_info_pair :: Function -> (Id, FunctionInfo)
function_to_info_pair f@(Function id args ins) =
	let ret_type = (case (last ins) of
		(ReturnInstruction (Just _)) -> type_basic_object
		(ReturnInstruction Nothing) -> type_void) in
		(id, FunctionInfo id args ret_type)

program_to_infos :: Program -> Map Id FunctionInfo
program_to_infos (Program fs) = Map.fromList (B.map function_to_info_pair fs)

id_to_variable_list ids = B.map (\id -> Variable id) ids

builtin1 = FunctionInfo "print_bool" ["o"] type_void
builtin2 = FunctionInfo "print_int" ["o"] type_void
builtin3 = FunctionInfo "print_float" ["o"] type_void
builtin4 = FunctionInfo "print_line" ["o"] type_void
builtin5 = FunctionInfo "print_string" ["o"] type_void

function_return_type :: Id -> Map Id FunctionInfo -> ClrType
function_return_type id infos = case (Map.lookup id infos) of
	Nothing -> type_void
	Just (FunctionInfo _ _ ret_type) -> if ret_type == type_void
		then type_void
		else type_basic_object

analyse_program :: Program -> C.State AnalysisState AnalysedProgram
analyse_program p@(Program fs) = 
	let s1 = (program_to_infos p) in
	let s2 = Map.insert "print_bool" builtin1 s1 in
	let s3 = Map.insert "print_int" builtin2 s2 in
	let s4 = Map.insert "print_float" builtin3 s3 in
	let s5 = Map.insert "print_line" builtin4 s4 in
	let s6 = Map.insert "print_string" builtin5 s5 in do
	analysed <- forM fs (analyse_function s6)
	return (AnalysedProgram analysed)

analyse_function :: Map Id FunctionInfo -> Function -> C.State AnalysisState AnalysedFunction
analyse_function infos f@(Function id args ins) =
	if not (check_return_instruction ins && is_return_instruction (last ins))
		then do 
			add_error (id ++ ": Last instruction in function should be return")
			return (AnalysedFunction "" type_void [] [] [])
		else 
	let locals = function_locals f in
	let ret_type = (case (last ins) of
		(ReturnInstruction (Just _)) -> type_basic_object
		(ReturnInstruction Nothing) -> type_void) in
--		_ -> error "Last instruction in function should be return") in
		return (AnalysedFunction id ret_type args (id_to_variable_list (Set.toList locals)) (analyse_instruction_list ins locals infos))

analyse_instruction_list list locals infos = B.map (\ins -> analyse_instruction ins locals infos) list

analyse_instruction (ExpressionInstruction e) locals infos = ExpressionInstruction (analyse_expression e locals infos)
analyse_instruction (ReturnInstruction Nothing) locals infos = ReturnInstruction Nothing
analyse_instruction (ReturnInstruction (Just e)) locals infos = (ReturnInstruction (Just (analyse_expression e locals infos)))
analyse_instruction (IfInstruction cond then_case else_case) locals infos = IfInstruction (analyse_expression cond locals infos) (analyse_instruction_list then_case locals infos) (analyse_instruction_list else_case locals infos)
analyse_instruction (WhileInstruction cond body) locals infos = WhileInstruction (analyse_expression cond locals infos) (analyse_instruction_list body locals infos)

analyse_expression (VariableExpression id) locals infos =
	if (Set.member id locals)
	then (LocalExpression id)
	else (ArgumentExpression id)
analyse_expression (ConstantExpression e) locals infos = ConstantExpression e
analyse_expression (UnaryExpression a op) locals infos = (UnaryExpression (analyse_expression a locals infos) op)
analyse_expression (BinaryExpression a b op) locals infos = (BinaryExpression (analyse_expression a locals infos) (analyse_expression b locals infos) op)
analyse_expression (Assignment a b) locals infos = (Assignment (analyse_expression a locals infos) (analyse_expression b locals infos))
--analyse_expression (FunctionCall id exps) locals = (FunctionCall id (analyse_expression_list exps locals))
analyse_expression (ArrayAccess a b) locals infos = (ArrayAccess (analyse_expression a locals infos) (analyse_expression b locals infos))
--analyse_expression (FunctionCall id exps) locals infos = (AnalysedFunctionCall type_void id (analyse_expression_list exps locals infos))
analyse_expression (FunctionCall id exps) locals infos =
	let ret_type = function_return_type id infos in
	--	then type_void
	--	else type_basic_object) in
		(AnalysedFunctionCall ret_type id (analyse_expression_list exps locals infos))

analyse_expression_list list locals infos = B.map (\e -> analyse_expression e locals infos) list

function_call_matches :: Expression -> Function -> Bool
function_call_matches (FunctionCall call_id call_args) (Function fun_id fun_args ins) =
	call_id == fun_id && length call_args == length fun_args

function_locals :: Function -> Set Id
function_locals (Function id args ins) =
	let args_set = Set.fromList args in
	let locals = Set.difference (Set.fromList (variables_from_instruction_list ins)) args_set in
		locals

variables_from_expr (ConstantExpression _) = []
variables_from_expr (UnaryExpression a _) = variables_from_expr a
variables_from_expr (BinaryExpression a b _) = variables_from_expr a ++ variables_from_expr b
variables_from_expr (VariableExpression id) = [id]
variables_from_expr (Assignment a b) = variables_from_expr a ++ variables_from_expr b
variables_from_expr (FunctionCall _ exps) = foldl (\ids exp -> ids ++ variables_from_expr exp) [] exps
variables_from_expr (ArrayAccess a b) = variables_from_expr a ++ variables_from_expr b

variables_from_instruction (ExpressionInstruction e) = variables_from_expr e
variables_from_instruction (ReturnInstruction Nothing) = []
variables_from_instruction (ReturnInstruction (Just e)) = variables_from_expr e
variables_from_instruction (IfInstruction cond then_case else_case) = variables_from_expr cond ++ variables_from_instruction_list then_case ++ variables_from_instruction_list else_case
variables_from_instruction (WhileInstruction cond body) = variables_from_expr cond ++ variables_from_instruction_list body

variables_from_instruction_list list = foldl (\ids ins -> ids ++ variables_from_instruction ins) [] list

is_lvalue (VariableExpression id) = True
is_lvalue _ = False

add_il :: String -> String -> String
add_il cur n = cur ++ "\n" ++ n
(++|) a b = add_il a b

data ClrTypeName = ClrVoid | ClrBool | ClrInt | ClrFloat | ClrString | ClrBasicObject | ClrBasicPrint | ClrClass deriving Eq
type ClrAssembly = String
data ClrType = ClrType { name:: ClrTypeName, assembly:: ClrAssembly, builtin:: Bool } deriving Eq

type_void = ClrType ClrVoid "" True
type_bool = ClrType ClrBool "" True
type_int = ClrType ClrInt "" True
type_float = ClrType ClrFloat "" True
type_string = ClrType ClrString "" True
type_basic_object = ClrType ClrBasicObject "'runtime'" False
type_basic_print = ClrType ClrBasicPrint "'runtime'" False

-- currently generated CLR class
type_class = ClrType ClrClass "" False

instance Show ClrTypeName where
	show ClrVoid = "void"
	show ClrBool = "bool"
	show ClrInt = "int32"
	show ClrFloat = "float"
	show ClrString = "string"
	show ClrBasicObject = "Basic.BasicObject"
	show ClrBasicPrint = "Basic.BasicPrint"
	show ClrClass = "Class"

instance Show ClrType where
	show (ClrType name assembly builtin) = (if builtin then "" else "class ") ++ (if assembly == "" then "" else "[" ++ assembly ++ "]") ++ show name

type ClassType = ClrType
type ArgumentType = ClrType
type MethodName = String

data IL = Ldc_i4 Integer | Ldc_r4 Float | Ldstr String
	| Ldloc Id | Stloc Id
	| Ldarg Id | Starg Id
	| Add | Sub | Mul | Div | Neg | Or | And
	| Newobj ClassType ArgumentType
	| Call ClassType MethodName [ClrType] ClrType
	| Pop | Ret
	| Dup
	| Label String
	| Br String | Brfalse String deriving Eq

-- CLR type BasicObject
emit_op :: Operator -> [IL]
emit_op BinaryAdd = [Call type_basic_object "op_Addition" [type_basic_object, type_basic_object] type_basic_object]
emit_op BinarySubtract = [Call type_basic_object "op_Subtraction" [type_basic_object, type_basic_object] type_basic_object]
emit_op BinaryMul = [Call type_basic_object "op_Multiply" [type_basic_object, type_basic_object] type_basic_object]
emit_op BinaryDiv = [Call type_basic_object "op_Division" [type_basic_object, type_basic_object] type_basic_object]
emit_op BinaryModulo = [Call type_basic_object "op_Modulus" [type_basic_object, type_basic_object] type_basic_object]
emit_op BinaryLess = [Call type_basic_object "op_LessThan" [type_basic_object, type_basic_object] type_bool] ++ [emit_cast_to_basic_object type_bool]
emit_op BinaryLessEqual = [Call type_basic_object "op_LessThanOrEqual" [type_basic_object, type_basic_object] type_bool] ++ [emit_cast_to_basic_object type_bool]
emit_op BinaryGreater = [Call type_basic_object "op_GreaterThan" [type_basic_object, type_basic_object] type_bool] ++ [emit_cast_to_basic_object type_bool]
emit_op BinaryGreaterEqual = [Call type_basic_object "op_GreaterThanOrEqual" [type_basic_object, type_basic_object] type_bool] ++ [emit_cast_to_basic_object type_bool]
emit_op BinaryEqual = [Call type_basic_object "op_Equality" [type_basic_object, type_basic_object] type_bool] ++ [emit_cast_to_basic_object type_bool]
emit_op BinaryNotEqual = [Call type_basic_object "op_Inequality" [type_basic_object, type_basic_object] type_bool] ++ [emit_cast_to_basic_object type_bool]
emit_op BinaryAnd = [Call type_basic_object "And" [type_basic_object, type_basic_object] type_basic_object]
emit_op BinaryOr = [Call type_basic_object "Or" [type_basic_object, type_basic_object] type_basic_object]
emit_op UnaryNegate = [Call type_basic_object "op_UnaryNegation" [type_basic_object] type_basic_object]

emit_constant (IntegerConstant i) = [Ldc_i4 i]
emit_constant (BooleanConstant b) = [if b then Ldc_i4 1 else Ldc_i4 0]
emit_constant (FloatConstant f) = [Ldc_r4 f]
emit_constant (StringConstant s) = [Ldstr s]

emit_constant_as_object c@(IntegerConstant i) = emit_constant c ++ [Newobj type_basic_object type_int]
emit_constant_as_object c@(BooleanConstant b) = emit_constant c ++ [Newobj type_basic_object type_bool]
emit_constant_as_object c@(FloatConstant f) = emit_constant c ++ [Newobj type_basic_object type_float]
emit_constant_as_object c@(StringConstant s) = emit_constant c ++ [Newobj type_basic_object type_string]

--function_return_type "print_bool" = type_void
--function_return_type "print_int" = type_void
--function_return_type "print_float" = type_void
--function_return_type "print_line" = type_void
--function_return_type "print_string" = type_void
--function_return_type id = type_basic_object

function_class "print_bool" = type_basic_print
function_class "print_int" = type_basic_print
function_class "print_float" = type_basic_print
function_class "print_line" = type_basic_print
function_class "print_string" = type_basic_print
-- user's functions
function_class id = type_class

-- leave expression's result on the stack, CLR type BasicObject
emit :: Expression -> [IL]
emit (ConstantExpression c) = emit_constant_as_object c
-- FIXME function arguments/local variables
--emit (VariableExpression id) = [Ldloc id]
emit (VariableExpression id) = error "invalid expression for emit"
emit (LocalExpression id) = [Ldloc id]
emit (ArgumentExpression id) = [Ldarg id]
emit (UnaryExpression a op) = emit a ++ emit_op op
emit (BinaryExpression a b op) = emit a ++ emit b ++ emit_op op
--emit (Assignment (VariableExpression id) b) = emit b ++ [Stloc id, Ldloc id]
emit (Assignment (LocalExpression id) b) = emit b ++ [Stloc id, Ldloc id]
emit (Assignment (ArgumentExpression id) b) = emit b ++ [Starg id, Ldarg id]
emit (Assignment _ _) = [] -- FIXME report an error-
-- FIXME check if expression tree is correct during analysis (if we're not trying to get value from void function)
-- this shouldn't get through semantic analysis
emit (FunctionCall id args) =
	let
		ret_type = type_void
		f_class = function_class id
		args_len = length args
		arg_types = take args_len (repeat type_basic_object)
	in
--		if function_return_type id == ClrVoid
--			then []
--			else
		emit_list args ++ [Call f_class id arg_types ret_type]
emit (AnalysedFunctionCall ret_type id args) =
	let
--		ret_type = function_return_type id
		f_class = function_class id
		args_len = length args
		arg_types = take args_len (repeat type_basic_object)
	in
		emit_list args ++ [Call f_class id arg_types ret_type]
--emit (ArrayAccess array index) = [Call "BasicObject" "

emit_list :: [Expression] -> [IL]
emit_list list = foldl (\il expr -> il ++ emit expr) [] list

-- emit for side effects
emit_side_effect :: Expression -> [IL]
--emit_side_effect e@(FunctionCall id args) = emit e ++ (if name (function_return_type id) == ClrVoid then [] else [Pop])
emit_side_effect (FunctionCall _ _) = error "emitting function call shouldn't happen"
emit_side_effect e@(AnalysedFunctionCall ret_type id args) = emit e ++ (if name (ret_type) == ClrVoid then [] else [Pop])
emit_side_effect e = emit e ++ [Pop]
--emit_side_effect (Constant i) = []
--emit_side_effect (BinaryExpression a b op) = emit_side_effect a ++ emit_side_effect b
--emit_side_effect (VariableExpression id) = []
--emit_side_effect (Assignment a b) = emit (Assignment a b) ++ [Pop]

next_label :: C.State Int String
next_label = do
	cur <- get
	put (cur + 1)
	return (show cur)

flatten :: [[a]] -> [a]
flatten [] = []
flatten [a] = a
flatten (x:xs) = x ++ flatten xs

emit_cast target_type = Call type_basic_object "op_Implicit" [type_basic_object] target_type
emit_cast_to_basic_object from_type = Newobj type_basic_object from_type

emit_instruction_list :: [Instruction] -> C.State Int [IL]
emit_instruction_list list = do
	list2 <- forM list emit_instruction
	return (flatten list2)

emit_instruction :: Instruction -> C.State Int [IL]
emit_instruction (ExpressionInstruction e) = do (return (emit_side_effect e))
emit_instruction (ReturnInstruction Nothing) = do (return ([Ret]))
emit_instruction (ReturnInstruction (Just e)) = do (return (emit e ++ [Ret]))
emit_instruction (IfInstruction cond then_case else_case) = do
	after_then_label <- next_label
	after_if_label <- next_label
	then_code <- emit_instruction_list then_case
	else_code <- emit_instruction_list else_case
	return (emit cond ++ [emit_cast type_bool, Brfalse after_then_label] ++ then_code ++ [Br after_if_label, Label after_then_label] ++ else_code ++ [Label after_if_label])
emit_instruction (WhileInstruction cond body) = do
	before_cond <- next_label
	after_body <- next_label
	body_code <- emit_instruction_list body
	return ([Label before_cond] ++ emit cond ++ [emit_cast type_bool, Brfalse after_body] ++ body_code ++ [Br before_cond, Label after_body])	

show_type_list [] = ""
show_type_list [t] = show t
show_type_list (t:ts) = show t ++ ", " ++ show_type_list ts

show_label t = "IL_" ++ t

instance Show IL where
	show (Ldc_i4 i) = "ldc.i4 0x" ++ showHex i ""
	show (Ldc_r4 f) = "ldc.r4 " ++ show f
	show (Ldstr s) = "ldstr " ++ "\"" ++ s ++ "\""
	show (Ldloc id) = "ldloc " ++ id
	show (Stloc id) = "stloc " ++ id
	show (Ldarg id) = "ldarg " ++ id
	show (Starg id) = "starg " ++ id
	show (Add) = "add"
	show (Sub) = "sub"
	show (Mul) = "mul"
	show (Div) = "div"
	show (Neg) = "neg"
	show (Newobj class_type arg_type) = "newobj instance void " ++ show class_type ++ "::'.ctor'(" ++ show arg_type ++ ")"
	show (Call class_type method_name arg_types ret_type) = "call " ++ show ret_type ++ " " ++ show class_type ++ "::" ++ method_name ++ "(" ++ show_type_list arg_types ++ ")"
	show (Pop) = "pop"
	show (Ret) = "ret"
	show (Label s) = show_label s ++ ":"
	show (Br t) = "br " ++ show_label t
	show (Brfalse t) = "brfalse " ++ show_label t

il_to_string :: [IL] -> String
il_to_string il = foldl (\s i -> s ++| show i) "" il

il_header :: String
il_header =
	".assembly extern mscorlib\n" ++
		"{\n" ++
		".ver 2:0:0:0\n" ++
		".publickeytoken = (B7 7A 5C 56 19 34 E0 89 )\n"++
		"}\n" ++
	".assembly extern 'runtime'" ++|
		"{" ++|
		".ver 0:0:0:0" ++|
		"}" ++|
        ".assembly 'vtable' { .hash algorithm 0x00008004 .ver 0:0:0:0 }\n"

il_class_header :: String -> String
il_class_header class_name = ".class private auto ansi beforefieldinit " ++
        class_name ++
        "\n extends [mscorlib]System.Object\n"

variable_to_declaration (Variable id) True = show type_basic_object ++ " " ++ id ++ ", "
variable_to_declaration (Variable id) False = show type_basic_object ++ " " ++ id

variables_to_declaration :: [Variable] -> String
variables_to_declaration [] = []
variables_to_declaration [x1] = variable_to_declaration x1 False
variables_to_declaration (x:xs) = variable_to_declaration x True ++ variables_to_declaration xs

show_arg id True = show type_basic_object ++ " " ++ id ++ ", "
show_arg id False = show type_basic_object ++ " " ++ id

show_args :: [Id] -> String
show_args [] = []
show_args [x1] = show_arg x1 False
show_args (x:xs) = show_arg x True ++ show_args xs

il_method_header :: Id -> [Id] -> ClrType -> String
il_method_header id args ret_type = ".method private static \n" ++
        "default " ++ show ret_type ++ " " ++ id ++ "(" ++ show_args args ++ ") cil managed\n"

il_method_locals :: [Variable] -> String
il_method_locals vars = ".locals init (" ++ variables_to_declaration vars ++ ")"

il_method_init_locals vars = il_to_string (foldl (\il (Variable id) -> il ++ emit (ConstantExpression (IntegerConstant 0)) ++ [Stloc id]) [] vars)

program_text (AnalysedProgram funs) =
	il_header ++|
        il_class_header "Class" ++|
	"{" ++|
		foldl (\str fun -> str ++| function_text fun) "" funs ++|
	"}"

function_text :: AnalysedFunction -> String
function_text (AnalysedFunction id ret_type args locals ins) =
	let entry_point = id == "main" in
                il_method_header id args ret_type ++|
                "{" ++|
                        (if entry_point then ".entrypoint" else " ") ++|
                        ".maxstack 1024" ++| -- Mono ignores it
			il_method_locals locals ++|
			il_method_init_locals locals ++|

				il_to_string (evalState (emit_instruction_list (ins)) 0) ++|
                "}"

write_to_file :: String -> String -> IO ()
write_to_file filename string = do
        file <- openFile filename (System.IO.WriteMode)
        hPutStr file string
        hClose file

parse_and_show :: IO ()
parse_and_show = do
	parseResult <- parseFromFile parse_program "fun.l"
	case parseResult of
		Left err -> do 
			putStr "Parse error\n"
			print err
		Right (Program fs) -> do
			putStr "Parsed without errors.\n"
			putStr (show (head fs))

main :: String -> IO ()
main filename = do
	parseResult <- parseFromFile parse_program filename
	case parseResult of
		Left err -> do 
			putStr "Parse error\n"
			print err
		Right p@(Program fs) -> do
			putStr "Parsed without errors.\n"
			let (analysed,state) = runState (analyse_program p) fresh_analysis_state in
				(do
					display_analysis_state state
					when (analysis_state_ok state) (do
						write_to_file "output.il" (program_text analysed)))
			putStr "Saved result to file.\n"
-}

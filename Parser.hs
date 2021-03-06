module Parser where

import Data.Char
import System.Environment
import System.IO
import Data.Set as Set
import Data.Map as Map
import Control.Monad.State.Strict as C

--import qualified LLVM.Core as Core
import Text.ParserCombinators.Parsec

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Expr as E
import Text.ParserCombinators.Parsec.Combinator

languageStyle = emptyDef
	{	commentStart = "/*"
	,	commentEnd = "*/"
	,	commentLine = "//"
	,	nestedComments = True
	,	identStart = letter
	,	identLetter = alphaNum <|> oneOf "_"
	,	reservedNames = ["return", "const", "var",
				"true", "false", "public",
				"private", "function", "class",
				"if", "then", "else",
				"while", "do", "for"]
	,	reservedOpNames = ["*", "/","+","-", "=",
				"<",">","<=",">=","==","!=",
				"and","or","(",")"]
	,	caseSensitive = True
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

runTest p input = parseTest (do {
			whiteSpace
			; x <- p
			; eof
			; return x
			}) input

parse_program :: Parser [Statement]
parse_program = do
	whiteSpace
	s <- many parse_global_statement
	eof
	return s

parse_global_statement :: Parser Statement
parse_global_statement =
	do
		do
			reserved "public"
			<|> reserved "private"
		parse_script_decl
	<|> do
		reserved "function"
		parse_script_fun
	<|> do
		reserved "class"
		parse_script_class
	<|> do
		do
			reserved "const"
			<|> reserved "var"
		parse_script_var
	<|>
		parse_statement
	<?>
		"global statement"

-- in original it returns list that contains mix of these 3 types
parse_block :: Parser Statement
parse_block =
	do
		braces $ do
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
	<?> "block"

parse_statement :: Parser Statement
parse_statement =
	parse_block
	<|> parse_if
	<|> parse_while
	<|> parse_do
	<|> parse_for
	<|> parse_return
	<|> parse_expr_stmt -- try might be needed
	<?> "statement"

parse_class = undefined

parse_function = undefined

parse_fun_expr = undefined

parse_local_class = undefined

parse_local_function = undefined

parse_local_variables = undefined

parse_return :: Parser Statement
parse_return =
	do
		reserved "return"
		(do
			semi
			return $ ReturnStatement Nothing
		 <|> do
			expr <- parse_expression
			semi
			return $ ReturnStatement $ Just expr)
	<?> "return"

-- FIXME else
parse_if :: Parser Statement
parse_if =
	do
		reserved "if"
		cond <- parse_expression
		reserved "then"
		block_true <- parse_block
		reserved "else"
		block_false <- parse_block
		return $ IfStatement cond block_true block_false
	<?>
		"if statement"

parse_while :: Parser Statement
parse_while =
	do
		reserved "while"
		cond <- parens parse_expression
		block <- parse_block
		return $ WhileStatement cond block
	<?>
		"while statement"

parse_do :: Parser Statement
parse_do =
	do
		reserved "do"
		block <- parse_block
		reserved "while"
		cond <- parens parse_expression
		return $ DoStatement cond block
	<?>
		"do while statement"

parse_for =
	do
		reserved "for"
		(init, before, after) <- parens $ do
			a <- parse_block
			semi
			b <- parse_block
			semi
			c <- parse_block
			return $ (a, b, c)
		body <- parse_block
		return $ ForStatement init before after body
	<?>
		"for statement"

parse_expression :: Parser Expression
parse_expression = E.buildExpressionParser table factor <?> "expression"

factor = parse_primitive -- wrong

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
create_assign_expr a b = AssignmentExpression a b

parse_expr_stmt :: Parser Statement
parse_expr_stmt =
	do
		expr <- parse_expression
		semi
		return $ ExpressionStatement expr

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
--	<|> do
--		id <- identifier
--		resolve id
	<?>
		"primitive"
		
parse_prefix :: Parser Expression
parse_prefix =
	do
		reserved "{"
		expr <- parse_expression
		reserved "}"
		return expr
	<|> do
		reserved "new"
		expr <- parse_expression
		return $ NewExpression expr
	<|> do
		parse_primitive

parse_primary = undefined

parse_script_decl :: Parser Statement
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

parse_script_class :: Parser Statement
parse_script_class =
	do
		return $ DeclarationStatement ClassDeclaration

parse_script_fun :: Parser Statement
parse_script_fun =
	do
		id <- identifier
		return $ DeclarationStatement FunctionDeclaration

parse_script_var =
	do
		id <- identifier
		return $ DeclarationStatement VariableDeclaration

data Constant = NullConstant | IntegerConstant Integer | BooleanConstant Bool | FloatConstant Float | StringConstant String deriving (Eq, Show)

data Operator = UnaryNegate | BinaryAdd | BinarySubtract | BinaryMul | BinaryDiv | BinaryLess | BinaryLessEqual | BinaryGreater | BinaryGreaterEqual | BinaryEqual | BinaryNotEqual | BinaryOr | BinaryAnd | BinaryModulo deriving (Eq, Show)

data Expression = ConstantExpression Constant | UnaryExpression Expression Operator | BinaryExpression Expression Expression Operator | AssignmentExpression Expression Expression | NewExpression Expression deriving (Eq, Show)

-- GADT for if, for, while for block?
data Statement = ExpressionStatement Expression | ReturnStatement (Maybe Expression) | IfStatement Expression Statement Statement | WhileStatement Expression Statement | DoStatement Expression Statement | ForStatement Statement Statement Statement Statement | DeclarationStatement Declaration | BlockStatement [Statement] deriving (Eq, Show)

data Declaration = FunctionDeclaration | VariableDeclaration | ConstantDeclaration | ClassDeclaration deriving (Eq, Show)

type Id = String

{-

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

-- FIXME
factor = parens expr
	<|> parse_constant
	<|> try parse_function_call
	<|> parse_variable
	<?> "simple expression"

-- FIXME: ambiguous
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

module TINYParser where

import Parsing

{-James Luxton 1190809 TINY language parser-}

{-Identifier, a literal-}
type Ide  =  String

{-Expression, a predifened expression for manipulating literals or expressions of literals i.e Zero = 0-}
data Exp  =  Zero | One | TT | FF | Read | I Ide | Not Exp | Equal Exp Exp | Plus Exp Exp deriving Show

{-Command, commands to manipulate how literals are interpreted and handle loop structures-}
data Cmd = Assign Ide Exp | Output Exp | IfThenElse Exp Cmd Cmd | IfThen Exp Cmd | WhileDo Exp Cmd | Seq Cmd Cmd deriving Show

expr :: Parser Exp
expr =  do 
        e1 <- term
        symbol "+"
        e2 <- expr
        return(Plus e1 e2)
        +++
        do
        e1 <- term
        symbol "="
        e2 <- term
        return (Equal e1 e2)
        +++
        term

term :: Parser Exp
term = do
       symbol "not"
       e <- expr
       return (Not e)
       +++
       factor

{-Make sure Parentheses are handled first or at least before identifiers-}
factor :: Parser Exp
factor = do
        symbol "("
        e <- expr
        symbol ")"
        return e
        +++
        do
        symbol "read"
        return Read
        +++
        do
        symbol "false"
        return FF
        +++
        do
        symbol "true"
        return TT
        +++
        do
        symbol "0"
        return Zero
        +++
        do
        symbol "1"
        return One
        +++
        do
        i <- identifier
        return (I i)

eparse :: String -> Exp
eparse xs = case (parse expr xs) of
            [(n,[])] -> n
            [(_,out)] -> error ("unused output " ++ out)
            [] -> error "invalid input"

cmd :: Parser Cmd
cmd = do
      c1 <- comp
      symbol ";"
      c2 <- cmd
      return (Seq c1 c2)
      +++
      comp

comp :: Parser Cmd
comp = do
       i <- identifier
       symbol ":="
       e <- expr
       return (Assign i e)
       +++
       do
       symbol "output"
       e <- expr
       return(Output e)
       +++
       do
       symbol "if"
       e <- expr
       symbol "then"
       c1 <- cmd
       symbol "else"
       c2 <- cmd
       return (IfThenElse e c1 c2)
       +++
       do
       symbol "while"
       e <- expr
       symbol "do"
       c <- cmd
       return (WhileDo e c)
       +++
       do 
       symbol "("
       c <- cmd
       symbol ")"
       return c

cparse :: String -> Cmd
cparse xs = case (parse cmd xs) of
            [(n,[])] -> n
            [(_,out)] -> error ("unused output " ++ out)
            [] -> error "invalid input"

textBookEx = "sum := 0; x := read; (while not (x = true) do (sum := sum + x; x := read));output sum" 
successOne = "output read + read; output 0"
successTwo = "sum := 0; n := read;j := 0; (while not (j = n) do sum := sum + j + 1; j := j + 1); output sum"
failureOne = "Unfomatted String"
failureTwo = "sum := 0; n := read;j := 0; (while not (j = n) do sum := sum + j + 1; j := j + 1); output"

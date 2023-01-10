## Introduction

This is my implementation of a MicroCaml lexer, parser, and interprerer. In principle, I have implemented the main fuctionality of how a statically-typed, interpreted language, like MicroCaml, is built behind the scenes. There are three parts I have implemeted: the lexer, parser, and interpreter.

## Lexer

The Lexer is implemted in the file "lexer.ml". In essence, the lexer takes in your input, lets say "1 + 2", for example. The output will be a list of tokens. In the example, the output would be [Tok_Int 1; Tok_Add; Tok_Int 2]. The lexer tokenizes your code so it can later be parsed. 

Most tokens only exist in one form (for example, the only way for `Tok_Concat` to appear in the program is as `^` and the only way for `Tok_Let` to appear in the program is as `let`). However, a few tokens have more complex rules. The regular expressions for these more complex rules are provided here:

- `Tok_Bool of bool`: The value will be set to `true` on the input string "true" and `false` on the input string "false".
  - *Regular Expression*: `true|false`
- `Tok_Int of int`: Valid ints may be positive or negative and consist of 1 or more digits. **Negative integers must be surrounded by parentheses** (without extra whitespace) to differentiate from subtraction (examples below). You may find the functions `int_of_string` and `String.sub` useful in lexing this token type.
  - *Regular Expression*: `[0-9]+` OR `(-[0-9]+)`
  - *Examples of int parenthesization*:
    - `tokenize "x -1" = [Tok_ID "x"; Tok_Sub; Tok_Int 1]`
    - `tokenize "x (-1)" = [Tok_ID "x"; Tok_Int (-1)]`
- `Tok_String of string`: Valid string will always be surrounded by `""` and **should accept any character except quotes** within them (as well as nothing). You have to "sanitize" the matched string to remove surrounding escaped quotes.
  - *Regular Expression*: `\"[^\"]*\"`
  - *Examples*:
    - `tokenize "330" = [Tok_Int 330]`
    - `tokenize "\"330\"" = [Tok_String "330"]`
    - `tokenize "\"\"\"" (* InvalidInputException *)`
- `Tok_ID of string`: Valid IDs must start with a letter and can be followed by any number of letters or numbers. **Note: Keywords may be substrings of IDs**.
  - *Regular Expression*: `[a-zA-Z][a-zA-Z0-9]*`
  - *Valid examples*:
    - "a"
    - "ABC"
    - "a1b2c3DEF6"
    - "fun1"
    - "ifthenelse"

MicroCaml syntax with its corresponding token is shown below, excluding the four literal token types specified above.

Token Name | Lexical Representation
--- | ---
`Tok_LParen` | `(`
`Tok_RParen` | `)`
`Tok_Equal` | `=`
`Tok_NotEqual` | `<>`
`Tok_Greater` | `>`
`Tok_Less` | `<`
`Tok_GreaterEqual` | `>=`
`Tok_LessEqual` | `<=`
`Tok_Or` | `\|\|`
`Tok_And` | `&&`
`Tok_Not` | `not`
`Tok_If` | `if`
`Tok_Then` | `then`
`Tok_Else` | `else`
`Tok_Add` | `+`
`Tok_Sub` | `-`
`Tok_Mult` | `*`
`Tok_Div` | `/`
`Tok_Concat` | `^`
`Tok_Let` | `let`
`Tok_Def` | `def`
`Tok_In` | `in`
`Tok_Rec` | `rec`
`Tok_Fun` | `fun`
`Tok_Arrow` | `->`
`Tok_DoubleSemi` | `;;`


## Parser

The Parser takes a stream of tokens and outputs as an Abstract Syntax Tree for the input expression.

Below is the AST type `expr`, which is returned by `parse_expr`. **Note** that the `environment` and `Closure of environment * var * expr` parts are only relevant to Project 4b, so you can ignore them for now.

```ocaml
type op = Add | Sub | Mult | Div | Concat | Greater | Less | GreaterEqual | LessEqual | Equal | NotEqual | Or | And

type var = string

type value =
  | Int of int
  | Bool of bool
  | String of string
  | Closure of environment * var * expr 

and environment = (var * value) list 

and expr =
  | Value of value
  | ID of var
  | Fun of var * expr (* an anonymous function: var is the parameter and expr is the body *)
  | Not of expr
  | Binop of op * expr * expr
  | If of expr * expr * expr
  | FunctionCall of expr * expr
  | Let of var * bool * expr * expr (* bool determines whether var is recursive *)
```

The Context Free Grammar (CFG) below describes the language of MicroCaml expressions. This CFG is right-recursive, so something like `1 + 2 + 3` will parse as `Add (Int 1, Add (Int 2, Int 3))`, essentially implying parentheses in the form `(1 + (2 + 3))`.) In the given CFG note that all non-terminals are capitalized, all syntax literals (terminals) are formatted `as non-italicized code` and will come in to the parser as tokens from the lexer. Variant token types (i.e. `Tok_Bool`, `Tok_Int`, `Tok_String` and `Tok_ID`) will be printed *`as italicized code`*.

- Expr -> LetExpr | IfExpr | FunctionExpr | OrExpr
- LetExpr -> `let` Recursion *`Tok_ID`* `=` Expr `in` Expr
  -	Recursion -> `rec` | ε
- FunctionExpr -> `fun` *`Tok_ID`* `->` Expr
- IfExpr -> `if` Expr `then` Expr `else` Expr
- OrExpr -> AndExpr `||` OrExpr | AndExpr
- AndExpr -> EqualityExpr `&&` AndExpr | EqualityExpr
- EqualityExpr -> RelationalExpr EqualityOperator EqualityExpr | RelationalExpr
  - EqualityOperator -> `=` | `<>`
- RelationalExpr -> AdditiveExpr RelationalOperator RelationalExpr | AdditiveExpr
  - RelationalOperator -> `<` | `>` | `<=` | `>=`
- AdditiveExpr -> MultiplicativeExpr AdditiveOperator AdditiveExpr | MultiplicativeExpr
  - AdditiveOperator -> `+` | `-`
- MultiplicativeExpr -> ConcatExpr MultiplicativeOperator MultiplicativeExpr | ConcatExpr
  - MultiplicativeOperator -> `*` | `/`
- ConcatExpr -> UnaryExpr `^` ConcatExpr | UnaryExpr
- UnaryExpr -> `not` UnaryExpr | FunctionCallExpr
- FunctionCallExpr -> PrimaryExpr PrimaryExpr | PrimaryExpr
- PrimaryExpr -> *`Tok_Int`* | *`Tok_Bool`* | *`Tok_String`* | *`Tok_ID`* | `(` Expr `)`

This CFG is what defines how MicroCaml code is made sense of. The parser looks for these specific patterns to make the code actually perform its desired operations. Without these rules, this would all be gibberish to the parser.

## Interpreter 

This is the last piece! The interpreter uses what the parser spits out to actually put everything together. When you run an expression such as "1 + 1", this is where the computer does the adding after it makes sense of what its supposed to do. The parser says what the interpreter should do, and the interpreter does the operations, such as adding 2 numbers. The whole idea is based on a principle called "operation semantics" to allow the interpreter to make out what the code is building up to.

### Running Mutop

You can run this together in *mutop* (Micro-utop), a version of `utop` for MicroCaml. Run the command `dune exec bin/mutop.exe` in your terminal or use the shell script `bash mutop.sh` to start the mutop toplevel. The toplevel uses your implementations for `parse_mutop` and `eval_mutop` to execute MicroCaml expressions. Here is an example of its use:

![Mutop Example](assets/ex.gif)

**Note:** If you are having issues running *mutop*, run the command `dune build` before starting the mutop toplevel.

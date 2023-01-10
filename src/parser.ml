open MicroCamlTypes
open Utils
open TokenTypes


(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok)) 
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h))) 
  
  (* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match
  
  (* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h
  
  (* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None
  
  
let rec parse_Expr toks = 
  match lookahead toks with
  | Some Tok_Let -> parse_LetExpr toks
  | Some Tok_If  -> parse_IfExpr toks
  | Some Tok_Fun -> parse_FunctionExpr toks
  | _ -> parse_OrExpr toks
      
and parse_LetExpr toks = 
  let post_let = match_token toks Tok_Let in
  match lookahead post_let with
  | Some Tok_Rec -> let post_rec = (match_token post_let Tok_Rec) in
      let x = match lookahead post_rec with
        | Some Tok_ID(x) -> x 
        | _ -> raise(InvalidInputException "Expected identifier after rec") in let post_equal = (match_token (match_token post_rec (Tok_ID(x))) Tok_Equal) in
      let (post_e1, e1) = parse_Expr post_equal in let (post_e2, e2) = parse_Expr (match_token post_e1 Tok_In) in
      (post_e2, Let (x, true, e1, e2))
  | Some Tok_ID(x) -> let post_equal = (match_token (match_token post_let (Tok_ID(x))) Tok_Equal) in
      let (post_e1, e1) = parse_Expr post_equal in let (post_e2, e2) = parse_Expr (match_token post_e1 Tok_In) in
      (post_e2, Let (x, false, e1, e2))
  | _ -> raise (InvalidInputException "Invalid let expression")
  
and parse_IfExpr toks =  
  let post_if = match_token toks Tok_If in
  let (post_e1, e1) = parse_Expr post_if in let (post_e2, e2) = parse_Expr (match_token post_e1 Tok_Then) in
  let (post_e3, e3) = parse_Expr (match_token post_e2 Tok_Else) in (post_e3, If(e1, e2, e3))
    
and parse_OrExpr toks = 
  let (post_and, and_e) = parse_AndExpr toks in match lookahead post_and with
  | Some Tok_Or -> let (post_or, or_e) = parse_OrExpr (match_token post_and Tok_Or) in
      (post_or, Binop(Or, and_e, or_e))
  | _ -> (post_and, and_e)
  
and parse_FunctionExpr toks = 
  let post_fun = (match_token toks Tok_Fun) in let id = (match lookahead post_fun with | Some Tok_ID(id) -> id | _ -> raise(InvalidInputException("Error"))) in
  let (post_e, e) = parse_Expr (match_token (match_token post_fun (Tok_ID(id))) Tok_Arrow) in
  (post_e, Fun(id, e))
  
and parse_AndExpr toks = 
  let (post_equality, equality_e) = parse_EqualityExpr toks in match lookahead post_equality with
  | Some Tok_And -> let (post_and, and_e) = parse_AndExpr (match_token post_equality Tok_And) in
      (post_and, Binop (And, equality_e, and_e))
  | _ -> (post_equality, equality_e)
      
and parse_EqualityExpr toks = 
  let (post_relational, relational_e) = parse_RelationalExpr toks in let rec helper op tok =
                                                                       let (post_equality, equality_e) = parse_EqualityExpr (match_token post_relational tok) in
                                                                       (post_equality, Binop (op, relational_e, equality_e)) in
  match lookahead post_relational with 
  | Some Tok_Equal -> helper Equal Tok_Equal
  | Some Tok_NotEqual -> helper NotEqual Tok_NotEqual
  | _ -> (post_relational, relational_e)
  
and parse_RelationalExpr toks = 
  let (post_additive, additive_e) = parse_AdditiveExpr toks in let rec helper op tok =
                                                                 let (post_relational, relational_e) = parse_RelationalExpr (match_token post_additive tok) in
                                                                 (post_relational, Binop (op, additive_e, relational_e)) in match lookahead post_additive with
  | Some Tok_Less -> helper Less Tok_Less
  | Some Tok_Greater -> helper Greater Tok_Greater
  | Some Tok_LessEqual -> helper LessEqual Tok_LessEqual
  | Some Tok_GreaterEqual -> helper GreaterEqual Tok_GreaterEqual
  | _ -> (post_additive, additive_e)
  
and parse_AdditiveExpr toks = 
  let (post_multiplicative, multiplicative_e) = parse_MultiplicativeExpr toks in let rec helper op tok = 
                                                                                   let (post_additive, additive_e) = parse_AdditiveExpr (match_token post_multiplicative tok) in
                                                                                   (post_additive, Binop (op, multiplicative_e, additive_e)) in match lookahead post_multiplicative with
  | Some Tok_Add -> helper Add Tok_Add
  | Some Tok_Sub -> helper Sub Tok_Sub
  | _ -> (post_multiplicative, multiplicative_e)
  
and parse_MultiplicativeExpr toks = 
  let (post_concat, concat_e) = parse_ConcatExpr toks in let rec helper op tok = 
                                                           let (post_multiplicative, multiplicative_e) = parse_MultiplicativeExpr (match_token post_concat tok) in
                                                           (post_multiplicative, Binop (op, concat_e, multiplicative_e)) in match lookahead post_concat with
  | Some Tok_Mult -> helper Mult Tok_Mult
  | Some Tok_Div -> helper Div Tok_Div
  | _ -> (post_concat, concat_e)  
      
and parse_ConcatExpr toks = 
  let (post_unary, unary_e) = parse_UnaryExpr toks in match lookahead post_unary with
  | Some Tok_Concat -> let (post_concat, concat_e) = parse_ConcatExpr (match_token post_unary Tok_Concat) in
      (post_concat, Binop (Concat, unary_e, concat_e))
  | _ -> (post_unary, unary_e)
  
and parse_UnaryExpr toks = 
  match lookahead toks with 
  | Some Tok_Not -> let (post_unary, unary_e) = parse_UnaryExpr (match_token toks Tok_Not) in
      (post_unary, Not(unary_e))    
  | _ -> parse_FunctionCallExpr toks
  
and parse_FunctionCallExpr toks = 
  let (post_prim1, pe1) = parse_PrimaryExpr toks in
  match lookahead post_prim1 with 
  | Some Tok_Int(x) -> let (post_prim2, pe2) = parse_PrimaryExpr post_prim1 in (post_prim2, FunctionCall(pe1, pe2))
  | Some Tok_Bool(x) -> let (post_prim2, pe2) = parse_PrimaryExpr post_prim1 in (post_prim2, FunctionCall(pe1, pe2))
  | Some Tok_String(x) -> let (post_prim2, pe2) = parse_PrimaryExpr post_prim1 in (post_prim2, FunctionCall(pe1, pe2))
  | Some Tok_ID(x) -> let (post_prim2, pe2) = parse_PrimaryExpr post_prim1 in (post_prim2, FunctionCall(pe1, pe2))
  | Some Tok_LParen -> let (post_prim2, pe2) = parse_PrimaryExpr post_prim1 in (post_prim2, FunctionCall(pe1, pe2))
  | _ -> (post_prim1, pe1)
  
and parse_PrimaryExpr toks = 
  match lookahead toks with 
  | Some Tok_Int(x) -> (match_token toks (Tok_Int x), Value(Int x))
  | Some Tok_Bool(x) -> (match_token toks (Tok_Bool x), Value(Bool x))
  | Some Tok_String(x) -> (match_token toks (Tok_String x), Value(String x))
  | Some Tok_ID(x) -> (match_token toks (Tok_ID x), ID x)
  | Some Tok_LParen -> let (post_e, e) = parse_Expr (match_token toks Tok_LParen) in (match_token post_e Tok_RParen, e)
  | _ -> raise (InvalidInputException "primary")    

let rec parse_expr toks = parse_Expr toks
  
let rec parse_mutop toks = 
  match lookahead toks with 
  | Some Tok_Def -> (let post_def = match_token toks Tok_Def in 
                     match lookahead post_def with 
                     | Some Tok_ID(id) -> let (post_e, e) = parse_expr (match_token (match_token post_def (Tok_ID(id))) Tok_Equal) in ((match_token post_e Tok_DoubleSemi), Def(id, e))
                     | _ -> raise (InvalidInputException "Missing valid def ID"))
  | Some Tok_DoubleSemi -> (match_token toks Tok_DoubleSemi, NoOp)
  | _ -> let (post_e, e) = (parse_expr toks) in (match_token post_e Tok_DoubleSemi, Expr(e))
  

    
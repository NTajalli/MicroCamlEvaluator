open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError


(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env
(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = match e with
  | Value(v) -> v
  | ID(id) -> eval_id env e
  | Fun(id, e1) -> eval_fun env e
  | Not(e1)  -> eval_not env e 
  | Binop(op, e1, e2) -> eval_binop env e
  | If(e1,e2,e3) -> eval_if env e
  | FunctionCall(e1, e2) -> eval_FunctionCall env e
  | Let(id,b,e1,e2) -> eval_let env e

and eval_id env e = match e with
  | ID(id) -> lookup env id
  | _ -> raise (TypeError("Expected ID"))

and eval_fun env e = match e with
  | Fun(id, e1) -> Closure(env, id, e1)
  | _ -> raise (TypeError("Expected Function"))

and eval_not env e = match e with 
  | Not(e1) ->
      (match e1 with
       | Not(e2) -> if (eval_not env e1) = (Bool true) then (Bool false) else (Bool true)
       | ID(id) -> (match (lookup env id) with
           | Bool(b1) -> Bool(not b1)
           | _ -> raise (TypeError("Expected Type Boolean after Not")))
       | Value(Bool(b1)) -> Bool(not b1)
       | _ -> raise (TypeError("Expected Type Not or ID")))
  | _ -> raise (TypeError("Expected Type Not or ID"))

and eval_binop env e =  match e with
  | Binop(Add, e1, e2) -> eval_arithmetic env e1 e2 Add
  | Binop(Sub, e1, e2) -> eval_arithmetic env e1 e2 Sub
  | Binop(Mult, e1, e2) -> eval_arithmetic env e1 e2 Mult
  | Binop(Div, e1, e2) -> eval_arithmetic env e1 e2 Div
  | Binop(Less, e1, e2) -> eval_relational env e1 e2 Less
  | Binop(Greater, e1, e2) -> eval_relational env e1 e2 Greater
  | Binop(LessEqual, e1, e2) -> eval_relational env e1 e2 LessEqual
  | Binop(GreaterEqual, e1, e2) -> eval_relational env e1 e2 GreaterEqual
  | Binop(Concat, e1, e2) -> eval_concat env e1 e2
  | Binop(Equal, e1, e2) -> eval_equality env e1 e2 Equal
  | Binop(NotEqual, e1, e2) -> eval_equality env e1 e2 NotEqual
  | Binop(Or, e1, e2) -> eval_logical env e1 e2 Or
  | Binop(And, e1, e2) -> eval_logical env e1 e2 And
  | _ -> raise(TypeError("Expected Valid Binop"))

and eval_if env e = match e with
  | If (e1, e2, e3) -> (match (eval_expr env e1) with
      | (Bool(true)) -> eval_expr env e2
      | (Bool(false)) -> eval_expr env e3
      | _ -> raise (TypeError("Expected If statement of type Boolan")))
  | _ -> raise (TypeError("Expected If statement"))

and eval_FunctionCall env e = match e with
  | FunctionCall(e1, e2) -> (match (eval_expr env e1) with
      | Closure(c_env, id, e) -> eval_expr (extend c_env id (eval_expr env e2)) e
      | _ -> raise (TypeError("Expected Closure from Function Call")))
  | _ -> raise (TypeError("Expected Function Call"))  

and eval_let env e = match e with
  | Let (id, is_rec, e1, e2) -> if is_rec then 
        let tmp_env = extend_tmp env id in
        update tmp_env id (eval_expr tmp_env e1);
        eval_expr tmp_env e2 
      else eval_expr (extend env id (eval_expr env e1)) e2
  | _ -> raise (TypeError("Expected e to be of type Let"))

and eval_arithmetic env e1 e2 op = match (eval_expr env e1) with
  | Int(v1) -> (match (eval_expr env e2) with
      | Int(v2) -> let x = (match op with
          | Add -> v1 + v2
          | Sub -> v1 - v2
          | Mult -> v1 * v2
          | Div -> if v2 = 0 then raise(DivByZeroError) else v1 / v2
          | _ -> raise(TypeError("Expected arithmetic operator"))) in Int(x)
      | _ -> raise(TypeError("Expected Type Int for arithmetic operation")))
  | _ -> raise(TypeError("Expected Type Int for arithmetic operation"))

and eval_relational env e1 e2 op = match (eval_expr env e1) with
  | Int(v1) -> (match (eval_expr env e2) with
      | Int(v2) -> let x = (match op with 
          | Less -> if v1 < v2 then true else false
          | Greater -> if v1 > v2 then true else false
          | LessEqual -> if v1 <= v2 then true else false
          | GreaterEqual -> if v1 >= v2 then true else false
          | _ -> failwith "Will not happen") in Bool(x)
      | _ -> raise(TypeError("Expected Type Int for relational operation"))) 
  | _ -> raise(TypeError("Expected Type Int for relational operation"))

and eval_equality env e1 e2 op = let helper v1 v2 = match op with 
    | Equal -> v1 = v2
    | NotEqual -> v1 != v2
    | _ -> failwith "Will not happen" in
  match (eval_expr env e1) with
  | Int(v1) -> (match (eval_expr env e2) with
      | Int(v2) -> Bool(helper v1 v2)
      | _ -> raise(TypeError("Expected Type Int for equality operation")))
  | Bool(v1) -> (match (eval_expr env e2) with
      | Bool(v2) -> Bool(helper v1 v2)
      | _ -> raise(TypeError("Expected Type Bool for equality operation")))
  | String(v1) -> (match (eval_expr env e2) with
      | String(v2) -> Bool(helper v1 v2)
      | _ -> raise(TypeError("Expected Type String for equality operation")))  
  | _ -> raise(TypeError("Expected Valid Type for equality operation"))

and eval_concat env e1 e2 = match (eval_expr env e1) with
  | String(s1) -> 
      (match (eval_expr env e2) with
       | String(s2) -> String(s1 ^ s2)
       | _ -> raise (TypeError("Expected 2 Strings for concat (left side is not of Type String)")))
  | _ -> raise (TypeError("Expected 2 Strings for concat (right side is not of Type String)")) 

and eval_logical env e1 e2 op = 
  match e1 with
  | Value(Bool(b1)) ->(match e2 with
      | Value(Bool(b2)) -> Bool(match op with Or -> b1 || b2 | And -> b1 && b2 | _ -> failwith "Will not happen") 
      | Binop (_,_,_) -> (match (eval_binop env e2) with
          | Bool b2 -> if b1 || b2 then (Bool(true)) else (Bool(false))
          | _ -> raise (TypeError("Expected Expression of Type Boolean")))
      | _ -> raise (TypeError("Expected Expression of Type Boolean")))
  | Binop (_,_,_) -> (match (eval_binop env e1) with
      | Bool b1 -> (match e2 with
          | Value (Bool(b2)) -> Bool(match op with Or -> b1 || b2 | And -> b1 && b2 | _ -> failwith "Will not happen")
          | Binop (_,_,_) -> (match (eval_binop env e2) with
              | Bool b2 -> if b1 || b2 then (Bool(true)) else ((Bool false))
              | _ -> raise (TypeError("Expected Expression of Type Boolean")))
          | _ -> raise (TypeError("Expected Expression of Type Boolean")))
      | _ -> raise (TypeError("Expected Expression of Type Boolean")))
  | _ -> raise (TypeError("Expected Expression of Type Boolean")) 
  

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with
  | Def(id1, e1) -> 
      let tmp_env = extend_tmp env id1 in let id2 = (eval_expr tmp_env e1) in update tmp_env id1 id2;
      (tmp_env, Some id2)
  | Expr(e1) -> (env, Some (eval_expr env e1))
  | NoOp -> (env, None)
open TokenTypes
open Str

let is_keyword word = 
  List.mem word ["if"; "else"; "then"; "not"; "let"; "def"; "in"; "rec"; "fun"; "true"; "false"] 

let tokenize input =
  let size = (String.length input) in
  let rec helper index = 
    if index >= size then [] 
    else if Str.string_match (Str.regexp {|\( \|\\n\|\\t\)+|}) input index then helper (index + String.length (Str.matched_string input))
    else if Str.string_match (Str.regexp {|\"[^\"]*\"|}) input index then
      let found_string = (Str.matched_string input) in (Tok_String (String.sub found_string 1 ((String.length found_string) - 2)))::(helper (index + (String.length found_string)))
    else if Str.string_match (Str.regexp ({|[0-9]+\|(-[0-9]+)|})) input index then
      let num = (Str.matched_string input) in if num.[0] = '(' then (Tok_Int (int_of_string (String.sub num 1 ((String.length num) - 2))))::(helper (index + (String.length num))) else (Tok_Int (int_of_string num))::(helper (index + (String.length num)))
    else if Str.string_match (Str.regexp ("[a-zA-Z][a-zA-Z0-9]*")) input index && ((is_keyword (Str.matched_string input)) = false) then 
      let id = (Str.matched_string input) in (Tok_ID id)::(helper (index + (String.length id)))
    else if Str.string_match (Str.regexp ({|true\|false|})) input index then 
      if (Str.matched_string input) = "true" then (Tok_Bool true)::(helper (index + 4)) else (Tok_Bool false)::(helper (index + 5))
    else if Str.string_match (Str.regexp ("(")) input index then
      Tok_LParen::(helper (index + 1))
    else if Str.string_match (Str.regexp (")")) input index then
      Tok_RParen::(helper (index + 1))  
    else if Str.string_match (Str.regexp ("=")) input index then
      Tok_Equal::(helper (index + 1))  
    else if Str.string_match (Str.regexp ("<>")) input index then
      Tok_NotEqual::(helper (index + 2))
    else if Str.string_match (Str.regexp (">=")) input index then
      Tok_GreaterEqual::(helper (index + 2))  
    else if Str.string_match (Str.regexp ("<=")) input index then
      Tok_LessEqual::(helper (index + 2))    
    else if Str.string_match (Str.regexp (">")) input index then
      Tok_Greater::(helper (index + 1))    
    else if Str.string_match (Str.regexp ("<")) input index then
      Tok_Less::(helper (index + 1))    
    else if Str.string_match (Str.regexp ("||")) input index then
      Tok_Or::(helper (index + 2))    
    else if Str.string_match (Str.regexp ("&&")) input index then
      Tok_And::(helper (index + 2))    
    else if Str.string_match (Str.regexp ("not")) input index then
      Tok_Not::(helper (index + 3))   
    else if Str.string_match (Str.regexp ("if")) input index then
      Tok_If::(helper (index + 2))     
    else if Str.string_match (Str.regexp ("then")) input index then
      Tok_Then::(helper (index + 4))    
    else if Str.string_match (Str.regexp ("else")) input index then
      Tok_Else::(helper (index + 4))  
    else if Str.string_match (Str.regexp ("+")) input index then
      Tok_Add::(helper (index + 1))  
    else if Str.string_match (Str.regexp ("->")) input index then
      Tok_Arrow::(helper (index + 2))  
    else if Str.string_match (Str.regexp ("-")) input index then
      Tok_Sub::(helper (index + 1))    
    else if Str.string_match (Str.regexp ("*")) input index then
      Tok_Mult::(helper (index + 1)) 
    else if Str.string_match (Str.regexp ("/")) input index then
      Tok_Div::(helper (index + 1))    
    else if Str.string_match (Str.regexp ({|\^|})) input index then
      Tok_Concat::(helper (index + 1))    
    else if Str.string_match (Str.regexp ("let")) input index then
      Tok_Let::(helper (index + 3))        
    else if Str.string_match (Str.regexp ("def")) input index then
      Tok_Def::(helper (index + 3))  
    else if Str.string_match (Str.regexp ("in")) input index then
      Tok_In::(helper (index + 2))
    else if Str.string_match (Str.regexp ("rec")) input index then
      Tok_Rec::(helper (index + 3))          
    else if Str.string_match (Str.regexp ("fun")) input index then
      Tok_Fun::(helper (index + 3))     
    else if Str.string_match (Str.regexp (";;")) input index then
      Tok_DoubleSemi::(helper (index + 2))
    else
      raise (InvalidInputException "Lexing error") in
      helper 0;; 




      
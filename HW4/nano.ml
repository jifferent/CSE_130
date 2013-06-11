(* CSE 130: Programming Assignment 4
 * nano.ml
 * Author: Daniel Shipps (A10239760)
 * Date: 5/19/2013
 *)

exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False    
(*| Head of expr
| Tail of expr *)
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
(*	| Head ->
		Printf.sprintf "%s"
		h::t
		(exprToString h)
	| Tail ->
		Printf.sprintf "%s"
		h::t
		(exprToString t) *)
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

(*
	lookup: string * env -> value 
	finds the most recent binding for a variable (i.e. the first from the left)
	in the list representing the environment.
*)

let lookup (x,evn) = match listAssoc (x,evn) with
	| Some x -> x
	| _ -> raise (MLFailure ("Variable Not Bound: " ^ x));;

(*
	eval : env * expr -> value
	when called with the pair (evn,e) evaluates an ML-nano expression e of the 
	above type, in the environment evn , and raises an exception MLFailure 
	("variable not bound: x") if the expression contains an unbound variable.
*)	
	
let rec eval (evn,e) = match e with
	| Const x -> Int x 
	| Var x -> lookup(x,evn)
	| True -> Bool true
	| False -> Bool false
	| Head -> h::t h
	| Tail -> h::t t
	| If(p,t,f) -> let Bool cond = eval(evn,p) in
		if cond
			then eval(evn,t)
		else eval(evn,f)
	| Let(v,a,b) -> let evn = (v, eval(evn,a))::evn in eval(evn,b)
	| Letrec(v,a,b) -> let evn = (v, eval(evn,a))::evn in eval(evn,b)
	| App(a,b) -> let arg = eval(evn,b) in
		begin match eval(evn,a) with
			| Closure(evn',None,x,e) -> eval((x,arg)::evn'@evn,e)
			| Closure(evn',Some n,x,e) as f -> eval((n,arg)::(x,f)::evn'@evn,e)
		end
	| Fun(x,e) -> Closure(evn,None,x,e)
	| Bin(a,op,b) ->
		let a = eval (evn,a) in
		let b = eval (evn,b) in
		begin match (a,op,b) with
            | Int a, Mul, Int b -> Int (a * b)
            | Int a, Div, Int b -> Int (a / b)
			
			| Int a, Plus, Int b -> Int (a + b)
            | Int a, Minus, Int b -> Int (a - b)
			
			| Int a, Lt, Int b -> Bool (a < b)
            | Int a, Le, Int b -> Bool (a <= b)
			| Int a, Ne, Int b -> Bool (a <> b)
			| Int a, Eq, Int b -> Bool (a = b)
			
			| Bool a, Ne, Bool b -> Bool (a <> b)
			| Bool a, Eq, Bool b -> Bool (a = b)

            | Bool a, And, Bool b -> Bool (a && b)
            | Bool a, Or, Bool b -> Bool (a || b)
           
		   | _ -> raise (MLFailure ("Invalid Operation"))
		end
	| _ -> raise (MLFailure ("Invalid Operation"))
(**********************     Testing Code  ******************************)

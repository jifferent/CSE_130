(*
 * expr.ml
 * cse130
 * based on code by Chris Stone
 * Author: Daniel Shipps (A10239760)
 * Date: 4/17/2013
 *)

(* REMEMBER TO DOCUMENT ALL FUNCTIOONS THAT YOU WRITE OR COMPLETE *)

type expr = 
    VarX
  | VarY
  | Sine     of expr
  | Cosine   of expr
  | Tangent  of expr
  | Average  of expr * expr
  | Times    of expr * expr
  | Triple   of expr * expr * expr
  | Thresh   of expr * expr * expr * expr	


  
(* exprToString : expr -> string
   exprToString matches the expression that is passed and
   returns a string of the expression. If nescesary 
   recursivly calles its self
*)

let rec exprToString e = match e with
	| VarX -> "x"
	| VarY -> "y"
	| Sine new_e -> "sin(pi*"^(exprToString new_e)^")"
	| Cosine new_e -> "cos(pi*"^(exprToString new_e)^")"
	| Average (new_e1,new_e2) -> "(("^(exprToString new_e1)^"+"
								^(exprToString new_e2)^")/2)"
	| Times (new_e1,new_e2) -> "("^(exprToString new_e1)^"*"
								^(exprToString new_e2)^")"
	| Thresh (new_e1,new_e2,new_e3,new_e4) -> "("^(exprToString new_e1)^"<"
								^(exprToString new_e2)^"?"
								^(exprToString new_e3)^":"
								^(exprToString new_e4)^")"
	| Triple (new_e1,new_e2, new_e3) -> "("^(exprToString new_e1)^"*"
								^(exprToString new_e2)^"*"^(exprToString new_e3)^")"
	| Tangent new_e -> "tan(pi*"^(exprToString new_e)^")";;

(* build functions:
     Use these helper functions to generate elements of the expr
     datatype rather than using the constructors directly.  This
     provides a little more modularity in the design of your program *)

let buildX()                       = VarX
let buildY()                       = VarY
let buildSine(e)                   = Sine(e)
let buildCosine(e)                 = Cosine(e)
let buildAverage(e1,e2)            = Average(e1,e2)
let buildTimes(e1,e2)              = Times(e1,e2)
let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)
let buildTriple(e1,e2,e3)          = Triple(e1,e2,e3)
let buildTangent(e)				   = Tangent(e)


let pi = 4.0 *. atan 1.0

(* eval : expr * float * float -> float
   eval matches the expression that is passed in and
   if nescesary recursivly calles its self
*)

let rec eval (e,x,y) = match e with
	| VarX -> x
	| VarY -> y
	| Sine new_e -> sin(pi *. eval(new_e,x,y))
	| Cosine new_e -> cos(pi *. eval(new_e,x,y))
	| Average (new_e1,new_e2) -> (( eval(new_e1,x,y) +. eval(new_e2,x,y) ) /. 2.0)
	| Times (new_e1,new_e2) -> (eval(new_e1,x,y) *. eval(new_e2,x,y))
	| Thresh (new_e1,new_e2,new_e3,new_e4) -> if eval(new_e1,x,y) < eval(new_e2,x,y)
												then eval(new_e3,x,y) 
											  else eval(new_e4,x,y)
	| Triple (new_e1,new_e2,new_e3) -> (eval(new_e1,x,y) *. eval(new_e2,x,y) *. eval(new_e3,x,y))
	| Tangent new_e -> tan(pi *. eval(new_e,x,y));;

(* (eval_fn e (x,y)) evaluates the expression e at the point (x,y) and then
 * verifies that the result is between -1 and 1.  If it is, the result is returned.  
 * Otherwise, an exception is raised.
 *)
let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
  assert (-1.0 <= rv && rv <= 1.0);
  rv

let sampleExpr =
      buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
      buildX()),buildTimes(buildCosine (buildCosine (buildAverage
      (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
      buildCosine (buildTimes (buildSine (buildCosine
      (buildY())),buildAverage (buildSine (buildX()), buildTimes
      (buildX(),buildX()))))))),buildY())))

let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))


(************** Add Testing Code Here ***************)

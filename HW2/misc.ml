(* CSE 130: Programming Assignment 2
 * misc.ml
 * Author: Daniel Shipps (A10239760)
 * Date: 4/17/2013
 *)

(* assoc : int * string * (string * int) list -> int 
   assoc takes a triple (d,k,l) as input. It searches the list l for a
   pair (kn,vn) where kn = k. If such a match exists vn is returned
   otherwise the default value d is returned. 
*)

let rec assoc (d,k,l) = match l with
	| [] -> d
	| (kn, vn)::tail -> 
		if k = kn 
			then vn
		else assoc (d,k,tail);;

(* removeDuplicates : int list * int list -> int list
   removeDuplicates takes a list as input and returns a copy of the list
   without any duplicates.
*)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | head::tail -> 
        let seen' = if (List.mem head seen) 
						then seen 
					else head::seen in
        let rest' = tail in 
	  helper (seen',rest') 
  in
      List.rev (helper ([],l));;


(* wwhile : (int -> int * bool) * int -> int
   wwhile takes a function f and an int b and calls f(b).
   The return value of f is the pair (b', c') where b' 
   is the modified input and c' is a bool. f(b') 
   continues to be called untill c' is false.
*)
let rec wwhile (f,b) = 
	let (b', c') = f(b) in
		if c' 
			then wwhile(f, b')
		else b';;
		
(* val fixpoint : ('a -> 'a) * 'a -> 'a = <fun>
   fixpoint repeatedly updates b with f(b) until 
   b=f(b) and then returns b.
*)
let fixpoint (f,b) = wwhile (
	(fun b -> if (f b = b) 
				then (f b, false) 
			  else (f b, true)), b)


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)

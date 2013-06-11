(* CSE 130: Programming Assignment 3
 * misc.ml
 * Author: Daniel Shipps (A10239760)
 * Date: 5/1/2013
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

(*
	sqsum : int list -> int 
	starts with a base value of 0 for the accumulator
	and then foldes left through the list multiplying each value by its self.
	Think of the accumulator as the carry from the multiplication.
*)

let sqsum xs = 
  let f a x = a + (x*x) in
  let base = 0 in
    List.fold_left f base xs;;

(*
	pipe : ('a -> 'a) list -> ('a -> 'a)
	pipe uses fold to make stacking calls to functions.
	It fist calls f1 on the base case and then continues calling
	functions on the results of the first. Thus f3(f2(f1(x))) 
*)	
	
let pipe fs = 
  let f a x = fun new_x -> x (a new_x) in
  let base = fun x -> x in
    List.fold_left f base fs;;

(*
	sepConcat : string -> string list -> -> string
	sepConcat takes a string and a list of strings and, using fold,
	takes the strings in the list and concats them with the "seperator"
	string between each one.
*)	
	
let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | head :: tail -> 
      let f a x = a ^ sep ^ x in
      let base = head in
      let l = tail in
        List.fold_left f base l;;
		
		
(*
	stringOfList : ('a -> string) -> 'a list -> string
	stringOfList uses sepConcat and List.map to return a string
	that is the concatenation of the the input function.
*)
		
let stringOfList f l = "[" ^ sepConcat "; " (List.map f l) ^ "]";;

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

(*
	clone : 'a -> int -> 'a list
	clone takes in two ints x and n. It returns a list containing
	n number of x. 
	clone 3 5;;
- : int list = [3; 3; 3; 3; 3] 
*)

let rec clone x n = match n with
	| n when n > 0 -> x :: clone x (n-1)
	| _ -> [];;

(*
	padZero : int list -> int list -> int list * int list
	padZero looks at the length of two input lists l1 and l2. It
	then takes the absolute value of the diference in length and 
	then uses clone to add that many zeros to the front of the 
	shorter list.
*)	
	
let rec padZero l1 l2 = 
	let number = abs(List.length l2 - List.length l1) in
	let padding = clone 0 number in
	if List.length l2 > List.length l1
		then padding @ l1, l2
	else l1, padding @ l2;;

(*
	removeZero : int list -> int list
	removeZero takes a list and splits it into a head and tail
	and if the head is zero it calls its self on the tail. Otherwise
	it does nothing. 
*)	
	
let rec removeZero l = match l with
	| head::tail -> if head = 0
						then removeZero tail
					else l
	| [] -> [];;

(*
	bigAdd : int list -> int list -> int list
	bigAdd takes in two lists and adds them together like they
	were just huge numbers. It does this by taking the accumulator
	and spliting it into a carry and result. It then takes the x and
	splits it into the x from each list. Then it simply adds them with 
	the carry. We then take this sum and devide it by 10 to get the int for
	the current spot in the list and mod it by 10 to get the carry (if any).
	
	Another very important part of this function is that it adds an additional
	zero to the front of each list. This way there can never be a carry on the last
	addition in the number. This lets enables the 
	
	let (_, res) = List.fold_left f base args in
      res
	  
	lines work. Otherwise the skeleton would need to look like 
	
	let (c, res) = List.fold_left f base args in
			c :: res
*)	
	
let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = 		
		let (c, res) = a in
		let (x1, x2) = x in
		let sum = x1 + x2 + c in
		(sum / 10, (sum mod 10)::res) in
    let base = (0, []) in
    let args = List.combine (List.rev (0::l1)) (List.rev (0::l2)) in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))

(*
	mulByDigit : int -> int list -> int list
	mulByDigit takes an int and a list of ints as input. 
	It then multiplies the int with each int in the list using fold.
	
	The accumulator is split in the same way as bigAdd.
	
	The product is calculated by taking the int, x, and multiplying it
	with the current list int, i. We also add in the carry form the last
	product calculation.
	
	The digit to place into the list is found with product mod 10 and the 
	carry is found with product / 10. Much in the same way that we did in bigAdd.
	
	In this version I did not adhear to the (_, res) format that was used in the skeletons
	of bigAdd and bigMul and instead used (c, res) because it is easier for me to wrap my
	head around.
*)	
	
let rec mulByDigit i l = 
	let multiply i l =
		let f a x =
			let (c, acc) = a in
			let product = (i * x) + c in
			let d = product mod 10 in
			let c = product / 10 in
			let res = d :: acc in
			c, res in
		let base = (0, []) in
		let args = List.rev l in
		let (c, res) = List.fold_left f base args in
			c :: res
	in
		removeZero (multiply i l);;

(*
	bigMul : int list -> int list -> int list
	bigMul takes two lists of ints as input. The goal is to treat these two
	lists like they are huge numbers and multiply them together. 
	
	At a high level what this function is doing is taking one int at a time from the
	first list and then calling mulByDigit on this int and the other list. This is then
	repeated for every int in the first list. We have to keep in mind that every time we shift
	over a place in the first list we need to append zeros to the back of the list obtained from
	mulByDigit, just as you would if you were multiplying two large numbers by hand. 
	
	The accumulator is split into a result and a "shift" value. This shift starts at zero and
	every "loop" through a zero is added to it. This is how the offset of addition is handled.
	
	The x is split into bigNum and digit. These are then fed into mulByDigit to generate a
	product. Finally bigAdd is called on this product and result. We then use fold to iterate 
	through the whole first list while applying these steps to each int in the list. 

*)

let bigMul l1 l2 = 
	let f a x =
		let (shift, res) = a in
		let (bigNum, digit) = x in
		let product = mulByDigit digit (bigNum @ (clone 0 shift)) in
		(shift + 1, bigAdd res product) in
  let base = (0, []) in
  let args = List.combine (clone l1 (List.length l2)) (List.rev l2) in
  let (_, res) = List.fold_left f base args in
    res

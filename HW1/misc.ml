(* CSE 130: Programming Assignment 1
 * misc.ml
 * Author: Daniel Shipps (A10239760)
 * Date: 4/11/2013
 *)

(* sumList : int list -> int 
   sumList takes an input parameter l and matches it with 1 of two cases. 
   Case 1: l is an empty list. If this happens return 0 as the sum.
   Case 2: l is a list of ints. If this happens add the head of the list
   to the sum of the tail of the list. The head will always be the first item
   in the list and the tail will be anything remaining in the list. Because of 
   Case 1 this can be zero if there are no more ints in the list. Recursion baby!
*) 

let rec sumList l = match l with
	| [] -> 0
	| head::tail -> head + sumList tail;;



(* digitsOfInt : int -> int list 
   digitsOfInt takes in int as input and if the value is positive it returns 
   the list of digits of n in the order in which they appear in n. If n is 0 or
   negative an empty list is returned. To achive this we mod n with 10 and concatinate
   it with an empty list and then append this list with a recursive call to digitsOfInt
   with n / 10 as the "new" n.
 *)

let rec digitsOfInt n = match n with
	| 0 -> []
	| n -> 
		if n < 0 then [] 
		else digitsOfInt (n / 10) @ (n mod 10) :: [];;


(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
 
let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)

(* additivePersistence : int -> int
   additivePersistence takes an interger as input. If the input is less than 10 it
   will have an additivePersistence of 0. Otherwise a call to our sumList function 
   will do the oberation required for finding the next "step" in the process of finding
   the additive persistence. The sum from sumList is then passed into a recursive call of
   additivePersistence. We get out final answer by the exprestion 1 + (additivePersistence sum)
   which looks like (1 + 1 + 0) for the input 9876.
 *)

let rec additivePersistence n = match n with
	| n -> 
		if n < 10 then 0
		else let sum = sumList (digits n) in 1 + (additivePersistence sum);;
		
		
(* digitalRoot : 'a -> 'b 
	digitalRoot takes an interger as input. The digital root of a number is
	The single digit obtained when finding the additive persistence of the input.
	If the input it less then 10 then the digital root is the number itself.
	Again sumList is used to add the digits of n and digitalRoot is called
	recursivly on this sum till it is less than 10.
 *)
	
let rec digitalRoot n = match n with
	| n ->
		if n < 10 then n
		else let sum = sumList (digits n) in digitalRoot sum;;
		
		
(* listReverse : 'a -> 'b 
	listReverse takes a list as input and returns the list flipped. (duh!)
	If an empty list is passed in we return an empty list to avoid errors
	associated with calling head and tail on an empty list.
	In order to reverse the list the head of the list is concatenated with
	the recursive call to (listReverse tail). This creates an expression 
	in the form [] @ [head3] @ [head2] @ [head1]
 *)

let rec listReverse l = match l with
	| [] -> []
	| (head::tail) -> (listReverse tail) @ [head];;

(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

(* palindrome : string -> bool 
	palindrome takes a string as input and returns true if the string is a 
	palindrome and false otherwise. 
	This function is fairly simple. It calles explode to create a list (called theList)
	from the string, then calles listReverse on a copy of the list, and
	finaly checks to see that these two lists are equal.
 *)
 
let palindrome w = let theList = explode w in theList = listReverse theList;;

(************** Add Testing Code Here ***************)

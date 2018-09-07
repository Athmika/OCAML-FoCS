
(* 
 * String <-> characters utility functions:
 *
 *   explode : string -> char list
 *      returns the list of characters making up a string
 *
 *   implode : char list -> string
 *      concatenates the list of characters into a string
 *
 *)

let explode (str) = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.get str index)::result)
  in
    acc(String.length(str)-1, [])

let implode cs = 
  List.fold_right (fun a r -> (String.make 1 a)^r) cs ""



(*
 *  The type of a finite automaton
 * 
 *)

type 'a fa = { states: 'a list;
               alphabet: char list;
               delta: ('a * char * 'a) list;
               start : 'a;
               final : 'a list }


(* 
 * Sample FAs
 *
 * The first accepts the language of all strings over {a,b} 
 * with a multiple-of-3 number of a's.
 *
 * The second accepts the language of all strings over {a,b,c} 
 * whose last three symbols are b's.
 *
 * Notes that states can be of any type -- the first example
 * uses strings as states, while the second uses integers.
 *
 *)

let faThreeA = { 
  states = ["start";"one";"two"];
  alphabet = ['a';'b'];
  delta = [ ("start",'a',"one");
	    ("one",'a',"two");
	    ("two",'a',"start");
	    ("start",'b',"start");
	    ("one",'b',"one");
	    ("two",'b',"two") ];
  start = "start";
  final = ["start"]
} 

let faLastThreeB = {
  states = [0;1;2;3];
  alphabet = ['a';'b';'c'];
  delta = [ (0,'a',0);
	    (0,'b',0);
	    (0,'c',0);
	    (0,'b',1);
	    (1,'b',2);
	    (2,'b',3); ];
  start = 0;
  final = [3]
} 





(* QUESTION 1 *)

let rec concatenate xs ys = 
    match xs with
    | [] -> ys
    | head::tail-> head::concatenate tail ys

let rec inS e xs = 
  match xs with
  | [] -> false
  | head::tail-> if head = e then true else inS e tail

let rec removingDuplicates xs = 
    match xs with
    | [] -> []
    | head::tail-> if not(inS head tail) then head::removingDuplicates tail else removingDuplicates tail

let rec unionS xs ys = 
    removingDuplicates (concatenate xs ys)

let rec subsetS xs ys = 
    match xs with
    | [] -> true
    | head::tail -> if inS head ys then subsetS tail ys else false

let rec hasFinal (m:'a fa) (qs:'a list):bool = 
   match qs with
   | [] -> false
   | head::tail -> if inS head m.final then true else hasFinal m tail

let rec reachableStatesHelper list1 q a=  
   match list1 with
           | [] -> []
           | head::tail -> match head with (s,l,e) ->
                             if s = q && l == a then e::reachableStatesHelper tail q a else reachableStatesHelper tail q a

let rec reachableStates (m:'a fa) (q:'a) (a:char):'a list = 
    reachableStatesHelper m.delta q a

let rec follow (m:'a fa) (qs:'a list) (a:char):'a list = 
 match qs with 
      |[]->[]
      |head::tail -> unionS (reachableStates m head a) (follow m tail a)

let rec followAll (m:'a fa) (qs:'a list) (syms:char list):'a list = 
  match syms with 
      | [] -> qs
      | symsx::symsx' -> followAll m (follow m qs symsx) symsx'


let accept (m:'a fa) (input:string):bool = 
  let inputList = explode input in 
  hasFinal m (followAll m [m.start] inputList)



(* QUESTION 2 *)

(* Right now, these are dummy finite automata -- replace by your own *)

let fa_q2_a  = { states = [0;1;2;3];
			alphabet = ['d';'e';'f'];
			delta = [(0,'d',0);
                (0,'e',0);
                (0,'f',1);
                (1,'f',1);
                (1,'d',2);
                (1,'e',2);
                (1,'f',2);
                (2,'d',3);
                (2,'e',3);
                (2,'f',3);
                ];
			start = 0;
			final = [3];}

let fa_q2_b  = { states = [0;1;2;3];
      alphabet = ['d';'e';'f'];
      delta = [(0,'d',0);
                (0,'e',0);
                (0,'f',1);
                (1,'f',1);
                (1,'f',1);
                (1,'e',2);
                (1,'f',2);
                ];
      start = 0;
      final = [1];}

let fa_q2_c  = { states = [0;1;2;3;4;5;6];
      alphabet = ['d';'e';'f'];
      delta = [(0,'d',1);
                (0,'f',6);
                (0,'e',3);
                (1,'f',1);
                (1,'e',3);
                (1,'d',2);
                (2,'d',5);
                (2,'e',3);
                (2,'f',2);
                (3,'e',4);
                (3,'f',3);
                (3,'d',1);
                (4,'e',5);
                (4,'d',1);
                (4,'f',4);
                (6,'f',6);
                (6,'d',1);
                (6,'e',3);
                ];
      start = 0;
      final = [1;2;3;4;6];}

let fa_q2_d  = { states = [0;1;2;3];
      alphabet = ['d';'e';'f'];
      delta = [(0,'e',1);
              (0,'f',0);
              (0,'d',3);
              (1,'e',0);
              (1,'f',1);
              (1,'d',2);
              (2,'e',3);
              (2,'f',2);
              (2,'d',1);
              (3,'e',2);
              (3,'f',3);
              (3,'d',0);
                ];
      start = 0;
      final = [1];}


let fa_q2_e  = { states = [0;1;2;3;4;5;6;7;8;9;10;11];
      alphabet = ['d';'e';'f'];
      delta = [(0,'e',1);
              (0,'d',3);
              (0,'f',6);
              (1,'e',0);
              (1,'d',2);
              (1,'f',7);
              (2,'d',1);
              (2,'f',5);
              (3,'d',0);
              (3,'e',2);
              (3,'f',4);
              (4,'d',6);
              (4,'e',5);
              (4,'f',8);
              (5,'e',4);
              (5,'d',7);
              (5,'f',9);
              (6,'e',7);
              (6,'d',4);
              (6,'f',10);
              (7,'e',6);
              (7,'d',5);
              (7,'f',11);
              (8,'d',10);
              (8,'e',9);
              (9,'e',8);
              (9,'d',11);
              (10,'e',11);
              (10,'d',8);
              (11,'d',9);
              (11,'e',10);
                ];
      start = 0;
      final = [11];}

let fa_q2_a : 'a fa = fa_q2_a


let fa_q2_b : 'a fa = fa_q2_b


let fa_q2_c : 'a fa = fa_q2_c


let fa_q2_d : 'a fa = fa_q2_d

let fa_q2_e : 'a fa = fa_q2_e




(* QUESTION 3 *)


(*
 *  Matrix multiplication (from HW1)
 * 
 *  A matrix is an int list list
 *
 *)

let multM m n = 
  let rec addV v w =
    match v with
    | [] -> []
    | x::xs -> (match w with
      | [] -> []
      | y::ys -> (x+y)::(addV xs ys))  in 
  let rec scaleV a v =
    match v with
    | [] -> []
    | x::xs -> (a*x)::(scaleV a xs)  in
  let rec mult1M v m = 
    match v with
    | [] -> []
    | x::xs -> (match m with
      | [] -> []   (* shouldn't happen *)
      | [ys] -> scaleV x ys
      | ys::yss -> addV (scaleV x ys) (mult1M xs yss))  in
  let rec multM' m n = 
    match m with 
    | [] -> []
    | xs::xss -> (mult1M xs n)::(multM' xss n)  in
  multM' m n

let rec matTransHelper m reachableStates list1 a=
     match list1 with
     |[]->[]
     |head::tail -> if inS head reachableStates then 1::(matTransHelper m reachableStates tail a) else 0::(matTransHelper m reachableStates tail a)

let rec matTransHelper1 m  statesLoop states  a =
match statesLoop with
   []->[]
   |head::tail -> (matTransHelper m (reachableStates m head a) states a) :: matTransHelper1 m tail states a

let matTrans (m:'a fa) (a:char):int list list =
  let input = m.states in
   matTransHelper1 m input input a

let rec matInitHelper start states = 
  match states with
  | [] -> []
  | head::tail-> if head = start then 1::matInitHelper start tail else 0::matInitHelper start tail

let matInit (m:'a fa):int list list =
  [matInitHelper m.start m.states] 

let rec matFinalHelper final states = 
  match states with
  | [] -> []
  | head::tail-> if inS head final then [1]::matFinalHelper final tail else [0]::matFinalHelper final tail

let matFinal (m:'a fa):int list list =
  matFinalHelper m.final m.states

let rec matAcceptHelper m transitions = 
  match transitions with
  | [] -> [[]]
  | [x] -> matTrans m x
  | head::tail -> multM (matTrans m head) (matAcceptHelper m tail)

let matAccept (m:'a fa) (str:string):bool =
  let transitions = explode str in
  if (multM (multM (matInit m) (matAcceptHelper m transitions)) (matFinal m) > [[0]]) then true else false 






(* This function is the base function that basically loops through all
 * strings  
 * of length up to n, and prints those that are accepted by the
 * finite automaton.
 *
 * This is being way too clever to try to not blow the stack 
 * while enumerating all strings up to a given length. Basically.
 * we enumerate all integer, convert them to base K (where K is the
 * size of the alphabet) and then replace every digit base K by the
 * letter of the alphabet at the corresponding index in the alphabet. 
 *
 * The key is that we can enumerate integers super easily
 *
 *)

let lang_ accept m n = 

  let rec expt a n = if n <= 0 then 1 else a*(expt a (n-1)) in
  
  let rec take n default l = 
    if n <= 0 then []
    else (match l with
          | [] -> default::(take (n-1) default l)
          | x::xs -> x::(take (n-1) default xs)) in
  
  let to_base_n base size n = 
    let rec loop n = 
      if n <= 0 then []
      else if n mod base = 0 then 0::(loop (n / base))
      else (n mod base)::(loop ((n - n mod base) / base))  in
    take size 0 (loop n)  in
  
  let to_string alphabet size n = 
    let base = List.length alphabet in
    let num_base = to_base_n base size n in
    implode (List.map (fun i -> List.nth alphabet i) num_base) in
  
    if n < 0 then ()
    else
      let print_str s = if s = "" then print_string "  <epsilon>\n"
  	              else print_string ("  "^s^"\n")  in
      let rec loop i = 
        if i <= n then 
  	  let ts = to_string m.alphabet i  in
  	  let bound = expt (List.length m.alphabet) i in
  	  let rec loop2 j = 
  	    if j < bound then (if accept m (ts j) 
                                 then print_str (ts j)
                               else ();
  			       loop2 (j+1))
  	    else ()  in
  	  (loop2 0; loop (i+1))
        else ()  in
      loop 0

(* 
 * Tester functions that dump the language accepted by a
 * finite automaton for Q1 and Q3
 *
 *)
 
let lang m n = lang_ accept m n
let matLang m n = lang_ matAccept m n

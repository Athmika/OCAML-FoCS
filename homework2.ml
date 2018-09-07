(* 

HOMEWORK 2

Name: Athmika Senthilkumar

Email: athmika.senthilkumar@students.olin.edu

Remarks, if any:

*)


(*
 *
 * Please fill in this file with your solutions and submit it
 *
 * The functions below are stubs that you should replace with your
 * own implementation.
 *
 * Always make sure you can #use this file before submitting it.
 * It has to load without any errors.
 *
 *)





   (* Q1: Set functions *)

let rec inS e xs = 
	match xs with
	| [] -> false
	| head::tail-> if head = e then true else inS e tail

let rec subsetS xs ys = 
    match xs with
    | [] -> true
    | head::tail -> if inS head ys then subsetS tail ys else false

let rec equalS xs ys = 
	if subsetS xs ys && subsetS ys xs then true else false 

let rec concatenate xs ys = 
    match xs with
    | [] -> ys
    | head::tail-> head::concatenate tail ys

let rec removingDuplicates xs = 
    match xs with
    | [] -> []
    | head::tail-> if not(inS head tail) then head::removingDuplicates tail else removingDuplicates tail

let rec unionS xs ys = 
    removingDuplicates (concatenate xs ys)

let rec interS xs ys = 
	match xs with
	| [] -> []
	| head::tail-> if inS head ys then head :: interS tail ys else interS tail ys 

let rec len xs =
	match xs with
	| [] -> 0
	| head::tail-> 1 + len tail

let rec sizeS xs = 
  len (removingDuplicates xs)


   (* Q2: Language functions *)


let rec atMost n xs = 
  match xs with
  | [] -> []
  | head::tail -> if String.length head <= n then head :: atMost n tail else atMost n tail

let rec unionL n xs ys = 
  atMost n (unionS xs ys)


let rec concatLHelper1 x ys = 
    match ys with
    | [] -> []
    | head::tail-> (x^head)::concatLHelper1 x tail

let rec concatL n xs ys = 
    match xs with
    | [] -> []
    | head::tail-> atMost n (unionS (concatLHelper1 head ys) (concatL n tail ys))

(*let rec recursiveConcat n xs = 
	if xs = [] then [] else unionL n (concatL n xs xs) (recursiveConcat n (concatL n xs xs))*)

(*let rec starLHelper n xs xs' = 
	 if xs=[] then [] else concatL(n xs xs') *)

let rec starL n xs = 
	if (n=0 || xs = []) then [""] else  (unionL n (""::xs) (concatL n xs (starL (n-1) xs)))

type re = 
    Empty 
  | Unit 
  | Letter of string 
  | Plus of re * re 
  | Times of re * re 
  | Star of re

let lang n s = 
  let fromChar c = String.make 1 c in
  let explode s = 
    let rec loop i result = 
      if i < 0 then result
      else loop (i-1) (s.[i]::result) in
    loop (String.length s - 1) []  in
  let isalpha = function 'A'..'Z'|'a'..'z' -> true | _ -> false in
  let expect c cs = 
    match cs with 
      f::cs when f = c -> Some cs
    | _ -> None in
  let expect_alpha cs = 
    match cs with
      f::cs when isalpha f -> Some (f,cs)
    | _ -> None  in
  let rec parse_R cs = 
    match parse_R1 cs with
      None -> None
    | Some (r1,cs) -> 
        (match expect '+' cs with
           None -> Some (r1,cs)
         | Some cs -> 
             (match parse_R cs with
                None -> None
              | Some (r2,cs) -> Some (Plus(r1,r2),cs)))
  and parse_R1 cs = 
    match parse_R2 cs with
      None -> None
    | Some (r1,cs) -> 
        (match parse_R1 cs with
           None -> Some (r1,cs)
         | Some (r2,cs) -> Some (Times(r1,r2),cs))  
  and parse_R2 cs = 
    match parse_R3 cs with
      None -> None
    | Some (r1,cs) -> 
        (match expect '*' cs with
           None -> Some (r1,cs)
         | Some cs -> Some (Star(r1),cs))
  and parse_R3 cs = 
    match expect_alpha cs with
      Some (a,cs) -> Some (Letter(fromChar(a)),cs)
    | None -> 
        (match expect '1' cs with
           Some cs -> Some (Unit, cs)
         | None -> 
             (match expect '0' cs with
                Some cs -> Some (Empty,cs)
              | None -> parse_parens cs))
  and parse_parens cs = 
    match expect '(' cs with
      None -> None
    | Some cs -> 
        (match parse_R cs with
           None -> None
         | Some (r,cs) -> 
             (match expect ')' cs with
                None -> None
              | Some cs -> Some (r,cs)))  in
  let parse s = 
    let cs = explode s in
    match parse_R cs with
      Some (re,[]) -> re
    | _ -> failwith ("Cannot parse "^s)  in
  let rec eval re = 
    match re with
      Empty -> []
    | Unit -> [""]
    | Letter (a) -> [a]
    | Plus (r1,r2) -> unionL n (eval r1) (eval r2)
    | Times (r1,r2) -> concatL n (eval r1) (eval r2)
    | Star r -> starL n (eval r)  in
  eval (parse s)

let show l = 
  let rec loop l seen = 
    match l with
    | [] -> ()
    | s::rest -> if List.mem s seen 
                    then loop rest seen 
                  else (match s with 
		        | "" -> (print_string "  <empty string>\n"; 
				 loop rest (""::seen))
			| s -> (print_string ("  "^s^"\n"); 
				loop rest (s::seen))) in
  loop l []



let regexp_a = "(d+e)(d+e)(d+e)(d+e)"

let regexp_b = "(((d+e)(d+e))*)(d+e)"

let regexp_c = "e*((d)(e*)(d))e*"

let regexp_d = "(de*d+e)*de*"

let regexp_e = "((e)*(dee)*e*)"

let a = "((a+b+c)(a+b+c))*"  (*strings of even length*)

let b = "((a+b+c)(a+b+c))*(a+b+c)"  (*strings of odd length*)

let c = "(a+b+c)*(abc)(a+b+c)*" (*string 'abc' appearing somewhere*)

let d = "(a+b+ac)*" (*a appears before the first c*)
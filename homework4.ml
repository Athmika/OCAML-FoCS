(* 
HOMEWORK 4

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
 * PLEASE DO NOT CHANGE THE TYPES IN THE STUBS I GIVE YOU. 
 * Doing so will make it impossible to test your code.
 *
 * Always make sure you can #use this file before submitting it.
 * It has to load without any errors.
 *
 *)




(* QUESTION 1 *)

let count (p:'a -> bool) (xs:'a list):int = List.fold_right (fun x re -> re+1) (List.filter p xs) 0 

let maxp (xs:int list):int = List.fold_right (fun x r -> if (x > r) then x else r) xs (0)
  
let mapf (fs:('a -> 'b) list) (xs:'a list):'b list  = List.fold_right (fun a b -> List.fold_right (fun x y-> x::y) a b) (List.fold_right (fun a b ->  ((List.fold_right(fun f y-> (f a)::y)) fs [])::b) xs []) []
 
let pairs (xs:'a list) (ys:'b list):('a * 'b) list  =  List.fold_right (fun t l -> List.fold_right (fun x y-> x::y) t l) (List.fold_right (fun c d -> (List.map (fun x -> (c,x)) ys)::d) xs []) []

let prepend (x:'a) (xss:'a list list):'a list list = List.fold_right (fun a b -> (x::a)::b) xss []

let prefixes (xs:'a list):'a list list = []::List.rev (List.fold_left (fun re x -> List.rev (x::List.rev ((fun re'-> match (re') with []->[] | head::tail -> head) re))::(re)) [] xs)
let prefixes' (xs:'a list):'a list list = List.rev (List.fold_left (fun re x -> List.rev (x::List.rev ((fun re'-> match (re') with []->[] | head::tail -> head) re))::(re)) [] xs)

let getfirst = (fun x -> match x with []->[]| head::tail -> head)
let getsecond = (fun x -> match List.rev x with []->[]| head::tail -> head)
let removefirst = (fun x -> match x with []->[]| head::tail -> tail)
let removelast = (fun x -> match List.rev x with []->[]| head::tail -> tail)
let flatten  = (fun x-> List.fold_right( fun a y->  ((getsecond a)@(getfirst a))::y ) x [])
let add = (fun x a -> List.rev (a::(List.rev x)))
let inject (x:'a) (xs:'a list):'a list list   = let prefixes = prefixes' xs in  (x::xs)::(removefirst (List.rev(flatten(List.fold_right (fun a' re' -> ((removefirst(getfirst(getfirst (re'))))::[a']) :: re')  (List.rev (List.fold_right(fun a re -> ((List.rev (x::List.rev a)))::re) prefixes [])) [[xs]]))))

(*let inject (x:'a) (xs:'a list):'a list list = *)
(* BONUS *)

let permutations (xs:'a list):'a list list = 
  failwith "permutations not implemented"
 


(*
 * Type for deterministic Turing machines
 *
 * Parameterized by type for states
 *)

type symbol = string

type 'a tm = { states : 'a list;
	       input_alphabet : symbol list;
	       tape_alphabet : symbol list;
	       left_marker : symbol;
	       blank : symbol;
	       delta : ('a * symbol) -> ('a * symbol * int);   (* 0 = Left, 1 = Right *)
	       start : 'a;
	       accept : 'a;
	       reject : 'a }

type 'a config = { state : 'a;
		   tape: symbol list;
		   position: int }


(* 
 * Some sample deterministic Turing machines
 *
 * asbs is the regular language {a^m b^n | m,n >= 0}
 * anbn is the non-regular language {a^n b^n | n >= 0}
 * anbncn is the non-regular language {a^n b^n c^n | n >= 0}
 *
 *)

let asbs = { states = ["start"; "q1"; "acc"; "rej"];
	     input_alphabet = ["a";"b"];
	     tape_alphabet = ["a";"b";"_";">"];
	     blank = "_";
	     left_marker = ">";
	     start = "start";
	     accept = "acc";
	     reject = "rej";
	     delta = (fun inp -> match inp with
	                 | ("start", "a") -> ("start", "a", 1)
     			 | ("start", "b") -> ("q1", "b", 1)
			 | ("start", ">") -> ("start", ">", 1)
			 | ("start", "_") -> ("acc", "_", 1)
			 | ("q1", "b") -> ("q1", "b", 1)
			 | ("q1", "_") -> ("acc", "_", 1)
			 | ("acc", "a") -> ("acc", "a", 1)
			 | ("acc", "b") -> ("acc", "b", 1)
			 | ("acc", ">") -> ("acc", ">", 1)
			 | ("acc", "_") -> ("acc", "_", 1)
			 | (_,c) -> ("rej",c,1))}

let anbn = { states = ["start"; "q1"; "q2"; "q3"; "q4"; "acc"; "rej"];
	     input_alphabet = ["a";"b"];
	     tape_alphabet = ["a";"b";"X";"/";"|"];
	     blank = "/";
	     left_marker = "|";
	     start = "start";
	     accept = "acc";
	     reject = "rej";
	     delta = (fun inp -> match inp with
	                 | ("start", "a") -> ("start", "a", 1)
     			 | ("start", "b") -> ("q1", "b", 1)
			 | ("start", "|") -> ("start", "|", 1)
			 | ("start", "/") -> ("q2", "/", 1)
			 | ("q1", "b") -> ("q1", "b", 1)
			 | ("q1", "/") -> ("q2", "/", 1)
			 | ("q2", "|") -> ("q3", "|", 1)
			 | ("q2", "a") -> ("q2", "a", 0)
			 | ("q2", "b") -> ("q2", "b", 0)
			 | ("q2", "X") -> ("q2", "X", 0)
			 | ("q2", "/") -> ("q2", "/", 0)
			 | ("q3", "X") -> ("q3", "X", 1)
			 | ("q3", "/") -> ("acc", "/", 1)
			 | ("q3", "a") -> ("q4", "X", 1)
			 | ("q4", "a") -> ("q4", "a", 1)
			 | ("q4", "X") -> ("q4", "X", 1)
			 | ("q4", "b") -> ("q2", "X", 1)
			 | ("acc", "a") -> ("acc", "a", 1)
			 | ("acc", "b") -> ("acc", "b", 1)
			 | ("acc", "|") -> ("acc", "|", 1)
			 | ("acc", "X") -> ("acc", "X", 1)
			 | ("acc", "/") -> ("acc", "/", 1)
			 | (_,c) -> ("rej",c,1))}


let anbncn = { states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"];
	       input_alphabet = ["a";"b";"c"];
	       tape_alphabet = ["a";"b";"c";"X";"_";">"];
	       blank = "_";
	       left_marker = ">";
	       start = "start";
	       accept = "acc";
	       reject = "rej";
	       delta = (fun inp -> match inp with
	                | ("start", "a") -> ("start", "a", 1)
     			| ("start", "b") -> ("q1", "b", 1)
			| ("start", "c") -> ("q6", "c", 1)
			| ("start", ">") -> ("start", ">", 1)
			| ("start", "_") -> ("q2", "_", 1)
			| ("q1", "b") -> ("q1", "b", 1)
			| ("q1", "c") -> ("q6", "c", 1)
			| ("q1", "_") -> ("q2", "_", 1)
			| ("q2", ">") -> ("q3", ">", 1)
			| ("q2", "a") -> ("q2", "a", 0)
			| ("q2", "b") -> ("q2", "b", 0)
			| ("q2", "c") -> ("q2", "c", 0)
			| ("q2", "_") -> ("q2", "_", 0)
			| ("q2", "X") -> ("q2", "X", 0)
			| ("q3", "X") -> ("q3", "X", 1)
			| ("q3", "_") -> ("acc", "_", 1)
			| ("q3", "a") -> ("q4", "X", 1)
			| ("q4", "a") -> ("q4", "a", 1)
			| ("q4", "X") -> ("q4", "X", 1)
			| ("q4", "b") -> ("q5", "X", 1)
			| ("q5", "b") -> ("q5", "b", 1)
			| ("q5", "X") -> ("q5", "X", 1)
			| ("q5", "c") -> ("q2", "X", 1)
			| ("q6", "c") -> ("q6", "c", 1)
			| ("q6", "_") -> ("q2", "_", 1)
		    | ("acc", "a") -> ("acc", "a", 1)
		    | ("acc", "b") -> ("acc", "b", 1)
		    | ("acc", "c") -> ("acc", "c", 1)
		    | ("acc", ">") -> ("acc", ">", 1)
		    | ("acc", "X") -> ("acc", "X", 1)
		    | ("acc", "_") -> ("acc", "_", 1)
			| (_,c) -> ("rej", c,1))}


      
(*
 * Helper functions
 *
 *   explode : string -> string list
 *      returns the list of symbols making up a string
 *
 *   printConfig: string tm -> string config -> 'a -> 'a
 *      print a configuration (including newline) to standard output
 *      and return a value
 * 
 *)

let explode (str:string):symbol list = 
  let rec acc index result = 
    if (index<0) then result
    else acc (index-1) ((String.sub str index 1)::result) in
  acc (String.length(str)-1) []


let printConfig (m:string tm) (c:string config) (value:'a):'a = 
    let mw = List.fold_right (fun a r -> max (String.length a) r) m.states 0 in
    let padding = max 0 (c.position + 1 - List.length c.tape) in
    let rec mkBlank k = match k with 0 -> [] | _ -> m.blank :: (mkBlank (k -1)) in
    let tape' = c.tape@(mkBlank padding) in
    let _ = print_string (String.sub (c.state^(String.make mw ' ')) 0 mw) in
    let _ = print_string "  "  in
    let _ = List.iteri (fun i sym -> 
                          if (i=c.position) then Printf.printf "[%s]" sym
			  else Printf.printf " %s " sym) tape'  in
    let _ = print_newline ()  in
    value



(* QUESTION 2 *)


let startConfig (m:'a tm) (w:string):'a config = {state = m.start; tape = m.left_marker::(explode w);position=0}

let acceptConfig (m:'a tm) (c:'a config):bool = if c.state = "acc" then true else false


let rejectConfig (m:'a tm) (c:'a config):bool = if c.state = "rej" then true else false

let rec replace_nth (xs:'a list) (n:int) (x:'a):'a list = 
match xs with
 | [] -> []
 | h::t -> if n=0 then x::replace_nth t (n-1) x else h:: replace_nth t (n-1) x

let rec nth n xs =
  match xs with
  | [] -> failwith "Out of bounds"
  | head::tail -> if n = 0 then head else nth (n-1) tail

(*let step (m:'a tm) (c:'a config):'a config =  m.delta (c.state,(nth c.position c.tape))*)
(*let step m c = m.delta (c.state,(nth c.position c.tape)) *)
let step (m:'a tm) (c:'a config):'a config =  let prefix = m.delta (c.state,(nth c.position c.tape)) in
 match prefix with
 | (x,y,z) -> {state = x; tape = if (c.position+1 >= List.length c.tape && z=1) then List.rev(m.blank::(List.rev (replace_nth c.tape (c.position) y))) else (replace_nth c.tape (c.position) y); position = if z= 1 then c.position+1 else c.position-1}

(*if x = "rej" then {state = x; tape=c.tape; position = if z= 1 then c.position+1 else c.position-1} else*)
(*(m:string tm) (w:string):bool*)
  let rec recurse m c =  printConfig m c (fun m x-> if acceptConfig m x then  true else if rejectConfig m x then false else recurse m (step m (x))) m c 

let run (m:string tm) (w:string):bool= 
  let startCon = startConfig m w in
  recurse m startCon
(* QUESTION 3 *)

let firstTM = { states = ["start";"s1";"q0";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"];
		input_alphabet = ["c";"d"];
		tape_alphabet = ["c";"d";"x";"y";"_";"<";"t";"p"];
		blank = "_";
		left_marker = "<";
		start = "start";
		accept = "acc";
		reject = "rej";
		delta = (fun inp -> match inp with 
			| ("q0", "c") -> ("q1", "x", 1)
			| ("start", "<") -> ("s1", "<", 1)
			| ("s1","c") -> ("q1","x",1)
			| ("s1","_") -> ("acc","_",1)
     	    | ("q0", "x") -> ("q0", "x", 1)
			| ("q0", "y") -> ("q0", "y", 1)
			| ("q0", "d") -> ("q3", "d", 0)
			| ("q1", "y") -> ("q1", "y", 1)
			| ("q1", "c") -> ("q1", "c", 1)
			| ("q1", "d") -> ("q2", "y", 0)
			| ("q2", "c") -> ("q2", "c", 0)
			| ("q2", "y") -> ("q2", "y", 0)
			| ("q2", "x") -> ("q0", "x", 1)
			| ("q3", "y") -> ("q3", "y", 0)
			| ("q3", "x") -> ("q4", "x", 1)
			| ("q4", "y") -> ("q5", "t", 1)
			| ("q4", "t") -> ("q4", "t", 1)
			| ("q4", "p") -> ("q4", "p", 1)
			| ("q4", "_") -> ("acc", "_", 1)
			| ("q5","y") -> ("q5","y",1)
			| ("q5","p") -> ("q5","p",1)
			| ("q5","d") -> ("q6","p",0)
			| ("q6","y") -> ("q6","y",0)
			| ("q6","p") -> ("q6","p",0)
			| ("q6","t") -> ("q4","t",1)
            | (_,c) -> ("rej",c,1))}


let secondTM = { states = ["start";"p1";"p2";"p4";"p5";"q0";"q1";"q2";"q3";"q4";"q5";"q6";"q7";"q8";"q9";"acc";"rej"];
		input_alphabet = ["c";"d"];
		tape_alphabet = ["c";"d";"_";"<"];
		blank = "_";
		left_marker = "<";
		start = "start";
		accept = "acc";
		reject = "rej";
		delta = (fun inp -> match inp with 
			| ("start", "<") -> ("start", "<", 1)
			| ("start", "c") -> ("p1", "c", 1)
			| ("start", "d") -> ("p1", "d", 1)
			| ("p2", "d") -> ("p4", "d", 0)
			| ("p2", "c") -> ("p4", "c", 0)
			| ("p4", "d") -> ("q0", "d", 0)
			| ("p4", "c") -> ("q0", "c", 0)
			| ("p1", "c") -> ("p2", "c", 1)
			| ("p1", "d") -> ("p2", "d", 1)
			| ("p2", "_") -> ("p5", "_", 0)
			| ("p5", "d") -> ("q5", "d", 0)
			| ("p5", "c") -> ("q5", "c", 0)
			| ("p6", "d") -> ("q5", "d", 0)
			| ("p6", "c") -> ("q6", "c", 0)
			| ("q0","d") -> ("q1","_",1)
			| ("q0","c") -> ("q1","_",1)
			| ("q1","d") -> ("q1","d",1)
			| ("q1","c") -> ("q1","c",1)
			| ("q1","_") -> ("q2","_",0)
			| ("q2","c") -> ("q3","_",0)
			| ("q2","d") -> ("q3","_",0)
			| ("q3","c") -> ("q4","c",0)
			| ("q3","d") -> ("q4","d",0)
			| ("q4","c") -> ("q9","c",0)
			| ("q4","d") -> ("q9","d",0)
			| ("q5","c") -> ("q6","c",1)
			| ("q5","d") -> ("q7","d",1)
			| ("q6","c") -> ("acc","c",1)
			| ("q7","d") -> ("acc","d",1)
			| ("q9","c") -> ("q8","c",0)
			| ("q9","d") -> ("q8","d",0)
			| ("q8","c") -> ("q8","c",0)
			| ("q8","d") -> ("q8","d",0)
			| ("q9","_") -> ("q5","_",1)
			| ("q8","_") -> ("q0","_",1)
            | (_,c) -> ("rej",c,1))}


let thirdTM = { states = ["start";"p1";"q0";"q1";"q2";"q3";"q4";"q5";"q6";"q7";"acc";"rej"];
		input_alphabet = ["0";"1";"#"];
		tape_alphabet = ["0";"1";"#";"_"];
		blank = "_";
		left_marker = "<";
		start = "start";
		accept = "acc";
		reject = "rej";
		delta = (fun inp -> match inp with 
			| ("start", "1") -> ("p1", "1", 1)
			| ("start", "0") -> ("p1", "0", 1)
			| ("start", "<") -> ("start", "<", 1)
			| ("p1", "0") -> ("q0", "0", 0)
			| ("p1", "1") -> ("q0", "1", 0)
			| ("p1", "#") -> ("q0", "#", 0)
			| ("q0","1")->("q1","x",1)
			| ("q0","0")->("q2","x",1)
			| ("q1","0")->("q1","0",1)
			| ("q1","1")->("q1","1",1)
			| ("q2","0")->("q2","0",1)
			| ("q2","1")->("q2","1",1)
			| ("q1","#")->("q3","#",1)
			| ("q2","#")->("q4","#",1)
			| ("q3","0")->("q5","y",0)
			| ("q3","y")->("q3","y",1)
			| ("q4","y")->("q4","y",1)
			| ("q4","1")->("q6","y",0)
			| ("q5","x")->("q0","x",1)
			| ("q5","0")->("q5","0",0)
			| ("q6","0")->("q6","0",0)
			| ("q5","1")->("q5","1",0)
		    | ("q6","#")->("q6","#",0)
			| ("q5","#")->("q5","#",0)
			| ("q5","y")->("q5","y",0)
			| ("q6","1")->("q6","1",0)
			| ("q6","y")->("q6","y",0)
			| ("q6","x")->("q0","x",1)
			| ("q0","#")->("q7","#",1)
			| ("q7","y")->("q7","y",1)
			| ("q7","_")->("acc","_",1)
            | (_,c) -> ("rej",c,1))}

let fourthTM = { states = ["start";"q0";"q1";"q2";"q3";"q4";"q5";"q6";"q7";"q8";"q9";"q10";"q11";"q12";"q13";"q14";"q15";"q16";"q17";"acc";"rej"];
		input_alphabet = ["0";"1";"#"];
		tape_alphabet = ["0";"1";"#";"x";"y";"_"];
		blank = "_";
		left_marker = "<";
		start = "start";
		accept = "acc";
		reject = "rej";
		delta = (fun inp -> match inp with 
			| ("start","<")->("start","<",1)
			| ("start","1")->("q1","x",1)
			| ("start","0")->("q2","x",1)
			| ("q1","0") -> ("q1","0",1)
			| ("q1","1") -> ("q1","1",1)
			| ("q2","0") -> ("q2","0",1)
			| ("q2","1") -> ("q2","1",1)
			| ("q2","#") -> ("q6","#",1)
			| ("q1","#") -> ("q3","#",1)
			| ("q3","y") -> ("q3","y",1)
			| ("q6","y") -> ("q6","y",1)
			| ("q3","0") -> ("q4","y",1)
			| ("q3","1") -> ("q5","y",1)
			| ("q6","0") -> ("q7","y",1)
			| ("q6","1") -> ("q8","y",1)
			| ("q4","1") -> ("q4","1",1)
			| ("q4","0") -> ("q4","0",1)
			| ("q5","1") -> ("q5","1",1)
			| ("q5","0") -> ("q5","0",1)
			| ("q6","1") -> ("q6","1",1)
			| ("q6","0") -> ("q6","0",1)
			| ("q8","1") -> ("q8","1",1)
			| ("q8","0") -> ("q8","0",1)
			| ("q4","#") -> ("q9","#",1)
			| ("q5","#") -> ("q10","#",1)
			| ("q7","#") -> ("q11","#",1)
			| ("q8","#") -> ("q12","#",1)
			| ("q9","z") -> ("q9","z",1)
            | ("q10","z") -> ("q10","z",1)
            | ("q11","z") -> ("q11","z",1)
            | ("q12","z") -> ("q12","z",1)
            | ("q9","0") -> ("q13","z",0)
            | ("q10","1") -> ("q14","z",0)
            | ("q11","0") -> ("q15","z",0)
            | ("q12","0") -> ("q16","z",0)
            | ("q13","0") -> ("q13","0",0)
            | ("q13","1") -> ("q13","1",0)
            | ("q13","#") -> ("q13","#",0)
            | ("q13","z") -> ("q13","z",0)
         
            | ("q13","y") -> ("q13","y",0)
            | ("q14","0") -> ("q14","0",0)
            | ("q14","1") -> ("q14","1",0)
            | ("q14","#") -> ("q14","#",0)
            | ("q14","z") -> ("q14","z",0)
 
            | ("q14","y") -> ("q14","y",0)
            | ("q15","0") -> ("q15","0",0)
            | ("q15","1") -> ("q15","1",0)
            | ("q15","#") -> ("q15","#",0)
            | ("q15","z") -> ("q15","z",0)
            
            | ("q15","y") -> ("q15","y",0)
            | ("q16","0") -> ("q16","0",0)
            | ("q16","1") -> ("q16","1",0)
            | ("q16","#") -> ("q16","#",0)
            | ("q16","z") -> ("q16","z",0)
            
            | ("q16","y") -> ("q16","y",0)
            | ("q13","x") -> ("start","x",1)
            | ("q14","x") -> ("start","x",1)
            | ("q15","x") -> ("start","x",1)
            | ("q16","x") -> ("start","x",1)
            | ("start","#") -> ("q17","#",1)
            | ("q17","y") -> ("q17","y",1)
            | ("q17","x") -> ("q17","x",1)
            | ("q17","z") -> ("q17","z",1)
            | ("q17","#") -> ("q17","#",1)
            | ("q17","_") -> ("acc","_",1)
            | (_,c) -> ("rej",c,1))
		}

let tm_q3_a : string tm = firstTM

let tm_q3_b : string tm = secondTM



(* QUESTION 4 *)

let tm_q4_not : string tm = thirdTM

let tm_q4_and : string tm = fourthTM

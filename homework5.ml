(* 

HOMEWORK 5

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
 * Do that in a _fresh_ OCaml shell 
 * It has to load without any errors.
 *
 *)




(* 
 * String <-> characters utility functions:
 *
 *   explode : string -> string list
 *      returns the list of characters making up a string
 *
 *)

let explode str = 
  let rec acc index result = 
    if (index<0) then result
    else acc (index-1) ((String.sub str index 1)::result) in
  acc (String.length(str)-1) []


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


(* Turing machines execution *)
let run_from_config (m:string tm) (c:string config):bool = 

  let printConfig m c value = 
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
    value in

  let acceptConfig (m:'a tm) (c:'a config):bool = 
    c.state = m.accept  in

  let rejectConfig (m:'a tm) (c:'a config):bool = 
    c.state = m.reject  in

  let rec replace_nth lst n s = 
    match lst with
    | [] -> []
    | x::xs when n = 0 -> s::xs
    | x::xs -> x::(replace_nth xs (n - 1) s)  in

  let step (m:'a tm) (c:'a config):'a config = 
    let l = List.length c.tape  in
    if (c.position < l) then
      match m.delta (c.state,List.nth c.tape c.position) with
      | (q,s,0) -> {state=q; tape = replace_nth c.tape c.position s; position = c.position-1}
      | (q,s,1) -> let new_tape = replace_nth c.tape c.position s  in
        if c.position + 1 < l then
 	  {state=q; tape = new_tape; position = c.position+1}
        else 
	  {state=q; tape = new_tape@[m.blank]; position = c.position+1}
    else
      failwith "Problem: not enough symbols on tape"  in

  let rec loop c = 
    if acceptConfig m c then true
    else if rejectConfig m c then false
    else 
      let c' = step m c in 
      loop (printConfig m c' c')  in

  loop (printConfig m c c)

let run (m:string tm) (w:string):bool = 
  run_from_config m { state= m.start;
                      tape= m.left_marker :: (explode w);
                      position= 0}



(*
 * Helper functions for creating structures states
 *
 *)

let rec tuples2 xs ys = 
  List.fold_right (fun x r -> (List.map (fun y -> (x,y)) ys)@r) xs []

let rec tuples3 xs ys zs = 
  List.fold_right (fun x r -> (List.map (fun (y,z) -> (x,y,z)) (tuples2 ys zs))@r) xs []

let rec tuples4 xs ys zs ws = 
  List.fold_right (fun x r -> (List.map (fun (y,z,w) -> (x,y,z,w)) (tuples3 ys zs ws))@r) xs []




(*
 * Sample Turing machine with structured states
 *
 * It accepts strings u#v where v is u+1 in binary 
 * representation
 *
 *)

let add1 = 
  { states = tuples3 ["start";"check1";"check2";"rewind";"go-end-1";"go-end-2";
		      "skip";"scan-1";"scan-2";"check-done"]
                     [99;0;1] [99;0;1];
    input_alphabet = ["0";"1";"#"];
    tape_alphabet = ["0";"1";"#";"X";"_";">"];
    blank = "_";
    left_marker = ">";
    start = ("start",99,99);
    accept = ("acc",99,99);
    reject = ("rej",99,99);
    delta = (fun x -> match x with
    | (("start",99,99),">") -> (("check1",99,99),">",1)
    | (("check1",99,99),"0") -> (("check1",99,99),"0",1)
    | (("check1",99,99),"1") -> (("check1",99,99),"1",1)
    | (("check1",99,99),"#") -> (("check2",99,99),"#",1)
    | (("check2",99,99),"0") -> (("check2",99,99),"0",1)
    | (("check2",99,99),"1") -> (("check2",99,99),"1",1)
    | (("check2",99,99),"_") -> (("rewind",99,1),"_",0)   (* start with a carry of 1! *)

    | (("rewind",99,carry),">") -> (("go-end-1",99,carry),">",1)
    | (("rewind",99,carry),"0") -> (("rewind",99,carry),"0",0)
    | (("rewind",99,carry),"1") -> (("rewind",99,carry),"1",0)
    | (("rewind",99,carry),"#") -> (("rewind",99,carry),"#",0)
    | (("rewind",99,carry),"X") -> (("rewind",99,carry),"X",0)

    | (("go-end-1",99,carry),"#") -> (("scan-1",99,carry),"#",0)
    | (("go-end-1",99,carry),sym) -> (("go-end-1",99,carry),sym,1)

    | (("scan-1",99,carry),"X") -> (("scan-1",99,carry),"X",0)
    | (("scan-1",99,carry),"0") -> (("skip",0,carry),"X",1)
    | (("scan-1",99,carry),"1") -> (("skip",1,carry),"X",1)
    | (("scan-1",99,0),">") -> (("check-done",99,99),">",1)  (* carry should be 0 to be done *)

    | (("skip",v,carry),"#") -> (("go-end-2",v,carry),"#",1)
    | (("skip",v,carry),"X") -> (("skip",v,carry),"X",1)

    | (("go-end-2",v,carry),"_") -> (("scan-2",v,carry),"_",0)
    | (("go-end-2",v,carry),sym) -> (("go-end-2",v,carry),sym,1)

    | (("scan-2",v,carry),"X") -> (("scan-2",v,carry),"X",0)
    | (("scan-2",v,carry),"0") when (v+carry) mod 2 = 0 -> (("rewind",99,(v+carry) / 2),"X",0)
    | (("scan-2",v,carry),"1") when (v+carry) mod 2 = 1 -> (("rewind",99,(v+carry) / 2),"X",0)

    | (("check-done",99,99),"_") -> (("acc",99,99),"_",1)
    | (("check-done",99,99),"X") -> (("check-done",99,99),"X",1)
    | (("check-done",99,99),"#") -> (("check-done",99,99),"#",1)

    | (_,sym) -> (("rej",99,99),sym,1))}





(* QUESTION 1 *)



let transformStates (states:'a list) (f:'a -> 'b):'b list = 
 List.map f states

let rec find_original (states:'a list) (f:'a -> 'b) (target:'b):'a =
  match states with
  | [] -> failwith "Orginal state cannot be found"
  | (x::x')-> if f x = target then x else find_original x' f target  

 
let transformDelta (states:'a list) (delta:'a * symbol -> 'a * symbol * int)
                      (f:'a -> 'c):'c * symbol -> 'c * symbol * int = 
                    
  (fun (x,y) -> (fun t -> match t with | (a,b,c) -> (f a,b,c) ) (delta ((find_original states f x),y)))
   

let transform (m:'a tm) (f:'a ->'b):'b tm = 
let re = 
  {
 	 states = transformStates m.states f;
    input_alphabet = m.input_alphabet;
    tape_alphabet = m.tape_alphabet;
    blank = m.blank;
    left_marker = m.left_marker;
    start = f m.start; 
    accept = f m.accept;
    reject = f m.reject;
    delta =	transformDelta m.states m.delta f; 
} in re 



(* QUESTION 2 *)
  


let perm n = 
	let rec f n = if n = 0 then [0] else n::f (n-1) in 
  { states = tuples3 ["q1";"q2";"q3";"count";"countrewind";"rewind";"acc";"rej";"_";">"] 
                     ["firstletter";"empty";"a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"] (List.rev (f n));
    input_alphabet = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"];
    tape_alphabet = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z";"X";"_";">";"#"];
    blank = "_";
    left_marker = ">";
    start = ("count","empty",0);
    accept = ("acc","empty",0);
    reject = ("rej","empty",0);
    delta = (fun x -> match x with

    
    | (("count","empty",0),">") -> (("count","empty",0),">",1)
    | (("count","empty",count),"#") when count < n -> let count = count+1 in (("count","empty",(count)),"#",1)
    | (("count","empty",count),"_") when count = n-1 ->   (("countrewind","empty",0),"_",0)
    | (("count","empty",count),"_") when count != n-1 ->   (("rej","empty",0),"_",1)
    | (("count","empty",count),sym) -> (("count","empty",(count)),sym,1)
    | (("countrewind","empty",0),">") -> (("q1","empty",0),">",1)
    | (("countrewind","empty",0),sym) -> (("countrewind","empty",0),sym,0)

    | (("q1","empty",0),">") -> (("q1","empty",0),">",1)
    | (("q1","empty",0),"#") -> (("q2","empty",0),"#",1)
    | (("q1","empty",0),"X") -> (("q1","empty",0),"X",1)
    | (("q1","empty",0),"_") -> (("acc","empty",0),"_",1)
    | (("q2","empty",0),"#") -> (("q2","empty",0),"#",1)
    | (("q2","empty",0),"_") -> (("acc","empty",0),"_",1)


    | (("q1","empty",0),firstletter) -> (("q2",firstletter,0),"X",1)
    | (("q2",firstletter,0),"#") -> (("q3",firstletter,0),"#",1)
    | (("q2",firstletter,0),"_") -> (("rewind","empty",0),"_",0)
    | (("q3",firstletter,0),secondletter) when secondletter = firstletter -> (("q2",firstletter,0),"X",1)
    | (("q3",firstletter,0),"_") -> (("rej","empty",0),"_",1)
    | (("q3",firstletter,0),sym) when sym != "_" -> (("q3",firstletter,0),sym,1)
    | (("q2",firstletter,0),sym) -> (("q2",firstletter,0),sym,1)
    | (("rewind","empty",0), ">")  -> (("q1","empty",0),">",1)
    | (("rewind","empty",0),sym) when sym != "<" -> (("rewind","empty",0),sym,0)
   
    | (_,sym) -> (("rej","empty",0),sym,1))}


let permutations (n:int):string tm = 
	let tr state =
    match state with
    | (x,y,z) -> x^"|"^y^"|"^(string_of_int z) in transform (perm n) tr



(* QUESTION 3 *)


(* Multitrack Turing machine (2 tracks) *)

type 'a tm_2 = { states_2 : 'a list;
		 input_alphabet_2 : symbol list;
		 tape_alphabet_2 : symbol list;
		 left_marker_2 : symbol;
		 blank_2 : symbol;
		 delta_2 : ('a * symbol * symbol) -> ('a * symbol * symbol * int);
                              (* 0 = Left, 1 = Right *)
		 start_2 : 'a;
		 accept_2 : 'a;
		 reject_2 : 'a }


let add1_2 = {
  states_2 = [ "start"; "skip#"; "skipX"; "copy0_skip"; "copy0"; "copy1_skip"; "copy1"; 
	       "done"; "add1"; "add-carry"; "allsame"; "acc"; "rej"; ];
  tape_alphabet_2 = [ "0"; "1"; "#"; "_"; ">" ];
  input_alphabet_2 = [ "0"; "1"; "#" ];
  blank_2 = "_"; 
  left_marker_2 = ">";
  start_2 = "start";
  accept_2 = "acc";
  reject_2 = "rej";

  delta_2 = (fun x ->
    match x with
    | ("start",">",b) -> ("skip#",">",b,1)

    | ("skip#","0",b) -> ("skip#","0",b,1)
    | ("skip#","1",b) -> ("skip#","1",b,1)
    | ("skip#","#",b) -> ("skipX","#",b,1)

    | ("skipX","X",b) -> ("skipX","X",b,1)
    | ("skipX","_",b) -> ("done","_",b,0)
    | ("skipX","0",b) -> ("copy0_skip","X",b,0)
    | ("skipX","1",b) -> ("copy1_skip","X",b,0)

    | ("copy0_skip",a,"_") -> ("copy0_skip",a,"_",0)
    | ("copy0_skip",a,b) -> ("copy0",a,b,1)

    | ("copy0",a,b) -> ("skip#",a,"0",1)

    | ("copy1_skip",a,"_") -> ("copy1_skip",a,"_",0)
    | ("copy1_skip",a,b) -> ("copy1",a,b,1)

    | ("copy1",a,b) -> ("skip#",a,"1",1)
	  
    | ("done","X",b) -> ("done","X",b,0)
    | ("done","#",b) -> ("add1","#",b,0)

    | ("add1","0","1") -> ("allsame","0","1",0)
    | ("add1","1","0") -> ("add1","1","0",0)

    | ("allsame","0","0") -> ("allsame","0","0",0)
    | ("allsame","1","1") -> ("allsame","1","1",0)
    | ("allsame",">",">") -> ("acc",">",">",1)

    | (_,a,b) -> ("rej",a,b,1)
	  
    )
} 


let split c s = 
  let rec loop ss = 
    try 
      let next = String.index ss c in
      let first = String.sub ss 0 next in
      let rest = String.sub ss (next+1) (String.length ss - next - 1) in
      first::(loop rest)
    with _ -> [ss]  in
  loop s



let transformDelta_2 (delta:'a * symbol * symbol -> 'a * symbol * symbol * int):
        'a * symbol -> 'a * symbol * int = 
    
let rec nth n xs =
  match xs with
  | [] -> failwith "Out of bounds"
  | head::tail -> if n = 0 then head else nth (n-1) tail in 

    (fun (x,y) -> (fun (a,b,c,d) -> (a,b^","^c,d)  ) (delta (x, nth 0 (split ',' y), nth 1 (split ',' y) )) )


 let rec con' a xs =
     match xs with 
     | [] -> [] 
     | (x::x') -> (a^","^x):: con' a x'

let rec con xs list1 =
	match xs with 
	| [] -> []
    | (x::x') -> (con' x list1) @ (con x' list1)


let transform_2 (m_2:'a tm_2):'a tm =
  let re = 
  {
 	 states = m_2.states_2;
    input_alphabet = List.map (fun x -> x^","^m_2.blank_2) m_2.input_alphabet_2;
    tape_alphabet = con m_2.tape_alphabet_2 m_2.tape_alphabet_2;
    blank = m_2.blank_2^","^m_2.blank_2;
    left_marker = m_2.left_marker_2^","^m_2.left_marker_2;
    start = m_2.start_2; 
    accept = m_2.accept_2;
    reject = m_2.reject_2;
    delta =	transformDelta_2 m_2.delta_2; 
} in re 

  


(* Helper function to run a 2-track Turing machine as a
   single track Turing machine *)
   
let run_2 (m_2:string tm_2) (s:string):bool = 
  let s_2 = List.map (fun s -> s^","^(m_2.blank_2)) (explode s)  in
  run_from_config (transform_2 m_2)
       { state = m_2.start_2;
         tape = (m_2.left_marker_2^","^m_2.left_marker_2)::s_2;
	 position = 0}


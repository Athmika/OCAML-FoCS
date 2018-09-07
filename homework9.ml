

module AbsStream :
  sig
      type 'a stream 
      val cst : 'a -> 'a stream
      val fby : 'a stream -> (unit -> 'a stream) -> 'a stream
      val map : ('a -> 'b) -> 'a stream -> 'b stream
      val map2 : ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream
      val filter : ('a -> 'b -> bool) -> 'a stream -> 'b stream -> 'b stream
      val split : 'a stream -> ('a stream * 'a stream)
      val print_stream : ('a -> string) -> int -> 'a stream -> unit
    end = 
  struct
    type 'a stream = R of 'a * (unit -> 'a stream)
    let memoize f = 
      let memoized = ref None in
      let new_f () = 
	match !memoized with
	| None -> let result = f () in memoized := Some result; result
	| Some v -> v   in
      new_f
    let mk h t = R (h, memoize t) 
    let unmk1 s = let R (h,t) = s in h
    let unmk2 s = let R (h,t) = s in t ()
    let rec cst v = mk v (fun () -> cst v)
    let fby s1 ps2 = mk (unmk1 s1) ps2
    let rec map f s = mk (f (unmk1 s)) (fun () -> map f (unmk2 s))
    let rec map2 f s1 s2 = mk (f (unmk1 s1) (unmk1 s2)) (fun () -> map2 f (unmk2 s1) (unmk2 s2))
    let rec filter p ctl s = if p (unmk1 ctl) (unmk1 s) then mk (unmk1 s) (fun () -> filter p (unmk2 ctl) (unmk2 s)) else filter p (unmk2 ctl) (unmk2 s)
    let split s = (cst (unmk1 s), unmk2 s)
    let rec zip s1 s2 = mk (unmk1 s1, unmk1 s2) (fun () -> zip (unmk2 s1) (unmk2 s2))
    let rec prefix n s = if n > 0 then (unmk1 s)::(prefix (n-1) (unmk2 s)) else []
    let print_stream tr n s =
      let rec loop n s = 
        if n > 0 then (print_string ((tr (unmk1 s))^" "); loop (n-1) (unmk2 s))
        else (print_string "...>\n") in
      print_string "< " ; loop n s
  end



(*
 * 
 * THESE ARE THE FUNCTIONS YOU GET TO USE
 *
 *)

type 'a stream = 'a AbsStream.stream
let cst : 'a -> 'a stream = AbsStream.cst
let fby : 'a stream -> (unit -> 'a stream) -> 'a stream = AbsStream.fby
let map : ('a -> 'b) -> 'a stream -> 'b stream = AbsStream.map
let map2 : ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream = AbsStream.map2
let filter : ('a -> 'b -> bool) -> 'a stream -> 'b stream -> 'b stream = AbsStream.filter
let split : 'a stream -> ('a stream * 'a stream) = AbsStream.split
let print_stream : ('a -> string) -> int -> 'a stream -> unit = AbsStream.print_stream


(*let rec fib () = fby (cst 0)
                        (fun () -> fby (cst 1)
                     (fun () -> let x = fib () in
                          add x (drop x)))*)



(* 
 *  Some helper functions to print a stream:
 *
 *  They are simple wrapper around print_stream
 *
 *)

let pri s = print_stream string_of_int 20 s
let prip s = print_stream (fun (x,y) -> "("^(string_of_int x)^","^(string_of_int y)^")") 20 s
let prs s = print_stream (fun x -> x) 20 s
let prf s = print_stream string_of_float 20 s


(* Some functions we saw in class *)

let rec gen_nats () = fby (cst 0)
                        (fun () -> (map (fun x -> x+1) (gen_nats ())))
let nats = gen_nats()

let evens = map (fun x -> 2*x) nats
let odds = map (fun x -> x+1) evens


(* some test streams *)

let s_ampl =
  let transf (v,(d,m)) =
    if d = 1 && v = m then (v-1,(-1,m))
    else if d = -1 && v = -m then (v+1,(1,m+1))
    else if d = 1 then (v+1,(1,m))
    else (v-1,(-1,m))  in
  let rec f () = fby (map2 (fun x y -> (x,y)) (cst 0) (cst (1,1)))
                     (fun () -> map transf (f ())) in
  map (fun (x,y) -> x) (f ())

let s_as = map (fun n -> "a"^(string_of_int n)) nats


(* 
 * QUESTION 1 
 * 
 *)

let scalef (n:float) (s:float stream):float stream =  map (fun x -> n*.x) s
let addf (s1:float stream) (s2:float stream):float stream = map2 (fun x y -> x+.y) s1 s2

let drop s = let (f,r) = split s in r
let rec psumsf (s:float stream):float stream =fby s (fun () -> addf (psumsf s) (drop s))

let scale (n:int) (s:int stream):int stream =  map (fun x -> n*x) s

let mult (s1:int stream) (s2:int stream):int stream = map2 (fun x y -> y*x) s1 s2


let zip (s1:'a stream) (s2:'b stream):('a * 'b) stream = map2 (fun x y -> (x,y)) s1 s2


let unzip (s:('a * 'b) stream):('a stream * 'b stream) =  (map (fun (x,y) -> x) s, map (fun (x,y) -> y) s)


let rec sumall() = fby (cst 0) (fun () -> map2 (fun x y -> x+y) nats (sumall()))

let funf f s1 s2 =  map2 (fun x y -> f x y) s1 s2

(*let rec fold (f:'a -> 'b -> 'b) (init_s:'b stream) (s:'a stream):'b stream =  funf f s (fby (map2 (fun x y -> f x y) s init_s) (fun () ->  fold f init_s s))*)

let rec fold (f:'a -> 'b -> 'b) (init_s:'b stream) (s:'a stream):'b stream =  (fby (map2 (fun x y -> f x y) s init_s) (fun () -> map2 (fun x y -> f x y) (drop s) (fold f init_s (s))))

(*map2 (fun x y -> f x y) (drop s)*) 
(*'b stream*)
(*map2 (fun x y -> f x y) s init_s*)

let check c x = if c > x then c else x 

let running_max (s:int stream):int stream = let (a,b) = split s in fold check a s


(*let rec helper (s:int stream):int stream = let (a,b) = split s in fby (cst(a))   (fun() -> map (fun x -> if x > (helper s) then x else (helper s)) s)*)

let double x = (x,x)

let rec stutter s = let (a,b) = split s in (fby a ) (fun () -> map (fun x -> let (a,b) = split x in a) (s) )
  (*(s:'a stream):'a stream*)




(*
 * QUESTION 2
 * 
 *)
let rec gen_nats' () = fby (cst 0.0)
                        (fun () -> (map (fun x -> x+.1.0) (gen_nats' ())))

let nats' = gen_nats'()

let evens' = map (fun x -> 2.0*.x) nats'
let odds' = map (fun x -> x+.1.0) evens'

let rec expt a b = 
   if b = 0.0 then 1.0 else expt a (b-.1.0)*.a;;

let stream1 z =  map2 (fun x y-> (-1.0) ** y *. (expt z x)/.x) (odds') (nats')
let stream2 z =  (fby (cst 0.0)) ( fun () -> map (fun x -> -.(expt z x)/.x) (map (fun x -> x+.2.0) odds'))

let rec arctan (z:float):float stream = psumsf((stream1 z))

(*let rec arctan (z:float):float stream = (fby (cst z)) (fun () -> map2(fun x y-> arctan z +. (expt z x)/.x -. (expt z y)/.y ) (map (fun x -> x+.4.0) odds') (map (fun x -> x+.2.0) odds'))
*)

let pi ():float stream = addf (scalef (16.0) (arctan (1.0/.5.0))) ( scalef (-.4.0) (arctan (1.0/.239.0)))

    
let rec newton (f:float -> float) (df:float -> float) (guess:float):float stream = (fby (cst guess)) (fun () -> map (fun x -> x -. (f x)/.(df x)) (newton f df guess))

let derivative (f:float -> float) (x:float):float stream = map (fun n -> (f (x +. (1.0/.n)) -. f x)/. (1.0/.n)) (drop nats')



let limit (epsilon:float) (s:float stream):float stream =  filter (fun a b -> abs_float(b -. a) < epsilon ) (drop s) (s) 
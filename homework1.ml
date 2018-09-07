

(* Question 1 *)


let rec expt a b = 
   if b = 0 then 1 else expt a (b-1)*a;;
   (*failwith "expt: not implemented"*)


let rec fastexpt a b =    
  if b=0 then 1 else if b mod 2 = 0 then expt a (b/2) * expt a (b/2) else expt a (b-1)*a;;
  (*failwith "fastexpt: not implemented"*)


let rec tetra a b = 
    if b = 0 then 1 else fastexpt a (tetra a (b-1));;
    (*failwith "tetra: not implemented"*)

  
let rec choose n k =
    if k = 0 then 1 else if n = k then 1 else choose (n-1) k + choose (n-1) (k-1);;
  

(* Question 2 *)


let rec doubleUp xs = 
    match xs with
      [] -> []
    | head::tail -> head :: head :: (doubleUp tail);;


let rec everyOther xs =
     match xs with
     []->[]
    |[x] -> [x]
    |head::second::tail -> head :: (everyOther tail)


  let rec concatenate xs ys = 
    match xs with
    | [] -> ys
    | head::tail-> head::concatenate tail ys

  
let rec concatenateAll xss =
    match xss with
    | [] -> []
    | head::tail ->  concatenate head (concatenateAll tail)

let rec nth n xs =
  match xs with
  | [] -> failwith "Out of bounds"
  | head::tail -> if n = 0 then head else nth (n-1) tail

let rec last xs = 
  match xs with
  [] -> failwith "List is empty"
  |[x] -> x
  |head::tail -> last(tail)

(* QUESTION 3 *)

let rec addV v w =
  match (v,w) with 
   ([x],[y]) -> [x+y]
  |(v::v',w::w') -> (v+w) :: addV v' w'
  |([],[]) -> []
  |([],_::_) -> failwith "Dimensions don't match"
  |(_::_,[]) -> failwith "Dimensions don't match"


let rec scaleV a v =
  match v with
  | [] -> []
  | [v] -> [a*v]
  | v::v' -> a*v :: scaleV a v' 


let rec inner v w =
  match (v,w) with 
   ([x],[y]) -> x*y
  |(v::v',w::w') -> (v*w) + inner v' w'
  |([],[]) -> 0
  |([],_::_) -> failwith "Vector sizes don't match"
  |(_::_,[]) -> failwith "Vector sizes don't match"



let rec outer v w =
  match (v,w) with
   ([],[]) -> [] 
  |(_::_,[]) -> []
  |([],_::_) -> failwith "Dimensions don't work"
  |(x::x',y::y') -> scaleV y v :: outer v y'

(* QUESTION 4 *)

let rec addM m n =
  match (m,n) with
  |([],[]) -> []
  |([],_::_) -> failwith "Dimensions don't match"
  |(_::_,[]) -> failwith "Dimensions don't match"
  |(x::x',y::y') -> addV x y :: addM x' y'

let rec scaleM a m =
  match m with
  |[] -> []
  |(x::x') -> scaleV a x :: scaleM a x'


let rec mult1M rowV m = 
   match (rowV,m) with
  | ([],_::_) -> failwith "Dimensions don't allow for matrix multiplication"
  | (_::_,[]) -> failwith "Dimensions don't allow for matrix multiplication"
  |([x],[y]) -> scaleV x y
  | (x::x',y::y') -> addV (scaleV x y) (mult1M x' y')
  | ([],[]) -> failwith "Dimensions don't allow for matrix multiplication"

let rec multM m n = 
  match (m,n) with
   | ([],[]) -> []
   | ([],_::_) -> []
   | (x::x',y::y') -> mult1M x n :: multM x' n 
   | (_::_,[]) -> failwith "Dimensions don't allow for matrix multiplication"


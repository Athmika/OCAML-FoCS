(* 

HOMEWORK 6
-
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
 * Doing so risks make it impossible to test your code.
 *
 * Always make sure you can #use this file before submitting it.
 * Do that in a _fresh_ OCaml shell 
 * It has to load without any errors.
 *
 *)



(*************************************************************
 * Binary trees
 *
 *)
   
type 'a bintree =
  | Empty
  | Node of 'a * 'a bintree * 'a bintree

let sample = Node(10,Node(3,Node(7,Empty,Empty),
                            Node(5,Empty,Empty)),
                     Node(6,Node(99,Empty,
                                 Node(66,Empty,Empty)),
                          Empty))

let pbt bt =
  let rec loop bt depth = 
    match bt with
    | Empty -> ()
    | Node(n,left,right) ->
	(loop right (depth^"    ");
         print_endline (depth^(string_of_int n));
         loop left (depth^"    ")) in
  loop bt ""




(*
 * QUESTION 1
 *
 *)


let rec size (t:'a bintree):int =
     match t with 
     | Empty -> 0
     | Node(n,left,right) -> 1+size(left)+size(right)

let rec height (t:'a bintree):int = 
match t with
| Empty -> 0
| Node(n,left,right) -> if (height(left) > height(right)) then 1+height(left) else 1+height(right)

let rec sum (t:int bintree):int = 
  match t with
| Empty -> 0
| Node(n,left,right) -> n+sum(left)+sum(right)

let rec fringe (t:'a bintree):'a list = 
  match t with
| Empty -> []
| Node (n,Empty,Empty) -> [n] 
| Node(n,left,right) -> fringe(left)@fringe(right)


let rec map (f:'a -> 'b) (t:'a bintree):'b bintree = 
  match t with
| Empty -> Empty
| Node(n,left,right) -> Node(f n,map f left,map f right) 

let rec fold (f:'a -> 'b -> 'b -> 'b) (t:'a bintree) (b:'b):'b = 
match t with
| Empty -> b
| Node(n,left,right) -> f n (fold f left b) (fold f right b) 

let rec preorder (t:'a bintree):'a list = fold (fun n l r -> (n::l)@r) t []

let rec postorder (t:'a bintree):'a list = fold (fun n l r -> l@(List.rev (n::(List.rev r)))) t [] 

let rec inorder (t:'a bintree):'a list = fold (fun n l r -> l@(n::r)) t []



(*
 *  QUESTION 2 
 *
 *)


let rec bst_insert (t:'a bintree) (x:'a):'a bintree = 
  match t with
| Empty -> Node(x,Empty,Empty)
| Node(n,left,right) -> if x>n then Node(n,left,bst_insert right x) else Node(n,bst_insert left x,right)


let rec bst_lookup (t:'a bintree) (x:'a):bool =
  match t with
| Empty -> false
| Node(n,left,right) -> if x=n then true else if x>n then bst_lookup right x else bst_lookup left x 



let rec bstify' nodes =
  match nodes with 
  | [] -> Empty  
  | (x::x') -> (bst_insert (bstify' x') x) 

let rec bstify (t:'a bintree):'a bintree = 
  let nodes = inorder t in 
  bstify' nodes

let rotate_left (t:'a bintree):'a bintree = 
match t with
| Empty -> Empty
| Node(n,left,right) -> let b = match right with
             | Empty -> Empty
             | Node(n,left,right) -> left in
 let toppart = Node(n,left,b) in match right with 
                                |Empty -> Empty
                                |Node(n,left,right) -> Node(n,toppart,right)  

let rotate_right (t:'a bintree):'a bintree = 
match t with
| Empty -> Empty
| Node(n,left,right) -> let b = match left with
             | Empty -> Empty
             | Node(n,left,right) -> right in
 let toppart = Node(n,b,right) in match left with 
                                |Empty -> Empty
                                |Node(n,left,right) -> Node(n,left,toppart)  


let rec avl_insert' t = 
match t with
| Empty -> Empty       
| Node(n,left,right)->  let balanced = Node(n, avl_insert' left, avl_insert' right) in 
                        let leftHeight = height (avl_insert' left) in
                        let rightHeight =   height (avl_insert' right) in
                        if (leftHeight - rightHeight) > 1 then (rotate_right balanced) 
                        else if (rightHeight - leftHeight) > 1 then (rotate_left balanced)
                        else 
                        balanced



let avl_insert (t:'a bintree) (x:'a):'a bintree = 
  let nt = bst_insert t x in 
   avl_insert' nt 




(* 
 * QUESTION 3  (insert below when available)
 *
 *)


type exp = 

 (* PART A *)
 | Num of int
 | Ident of string
 | Plus of exp * exp
 | Times of exp * exp

 (* PART B *)
 | EQ of exp * exp
 | GT of exp * exp
 | And of exp * exp
 | Not of exp
 | If of exp * exp * exp

 (* PART C *)
 | Letval of string * exp * exp

 (* PART D - BONUS *)
 | Letfun of string * string * exp * exp
 | Letrecfun of string * string * exp * exp
 | App of string * exp


type 'a env = string -> 'a

let update (env:'a env) (x:string) (v:'a):'a env = 
  fun y -> if (y = x) then v else (env y)

let init:int env = (fun x -> 0)
let set = (fun p-> p)

let initF:(int -> int) env = (fun x -> failwith ("unknown function "^x))


let rec eval (exp:exp) (env:int env):int = 
  match exp with
  | Num x -> x
  | Ident (x) -> (env x)
  | Plus (x,y) -> ((eval x env) + (eval y env))
  | EQ (x,y) -> if (eval x env) = (eval y env) then 1 else 0
  | GT (x,y) ->if (eval x env) > (eval y env) then 1 else 0
  | And (x,y) -> if (((eval x env) != 0) && ((eval y env) != 0)) then 1 else 0
  | Not (x) -> if (eval x env) = 0 then 1 else 0
  | Times (x,y) -> ((eval x env) * (eval y env))
  | If(x,y,z) -> if (eval x env = 1) then (eval y env) else (eval z env) 
  | Letval(x,e1,e2) ->  (eval e2 (update env x (eval e1 env)))

let rec evalF (exp:exp) (env:int env) (fenv:(int -> int) env):int =
  failwith "evalF not implemented"
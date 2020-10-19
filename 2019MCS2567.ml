module Ls :
sig
val member : 'a -> 'a list -> bool
val call_set : 'a list -> 'a list -> 'a list
val set : 'a list -> 'a list
val emptyset : 'a list -> bool
val call_intersection : 'a list -> 'a list -> 'a list -> 'a list
val intersection : 'a list -> 'a list -> 'a list
val call_union : 'a list -> 'a list -> 'a list
val union : 'a list -> 'a list -> 'a list
val call_difference : 'a list -> 'a list -> 'a list -> 'a list
val difference : 'a list -> 'a list -> 'a list
val equal_cal : 'a list -> 'a list -> bool
val equal : 'a list -> 'a list -> bool
val call_subset : 'a list -> 'a list -> bool
val subset : 'a list -> 'a list -> bool
val power_cal : 'a list -> 'a list list
val power : 'a list -> 'a list list
val product_helper_func : 'a -> 'b list -> ('a * 'b) list -> ('a * 'b) list
val product_cal : 'a list -> 'b list -> ('a * 'b) list -> ('a * 'b) list
val product : 'a list -> 'b list -> ('a * 'b) list
end =
struct
(*--------------member: start --------------------*)
(*returns true if an elemen belongs to a set else false*)
let rec member x s = 
match s with
| [] -> false
| hd :: xs ->
if hd = x then true
else
member x xs
;;

(*--------------member: end ------------------------------*)
(*--------------set: start -------------------------------*)
(*Returns a set of unique elements*)
let rec call_set l s = 
  match l with
  | [] -> s
  | h1::t1 -> 
  if member h1 s = true then 
    call_set t1 s
  else
    call_set t1 (h1::s)
;;

(*Set calling function*)
let set l = call_set l [];;

(*--------------set: start -------------------------------*)

(*--------------emptyset: start -------------------------*)
(*return true if a set is empty else false*)
let emptyset s = 
match s with
| [] -> true
| hd::xs -> false
;; 
(*--------------emptyset: end------- --------------------*)

(*--------------Intersection: start --------------------*)
(*find intersection of two lists and returns a list*)
let rec call_intersection l1 l2 res = 
  match l1 with
    | [] -> List.rev res
    | h1 :: t1 ->
    (
      match l2 with
      | [] -> []
      | h2 :: t2 ->
      if member h1 l2 = true then 
        call_intersection t1 l2 List.(cons h1 res) 
      else 
        call_intersection t1 l2 res
    )
;;
(*calling function for intersection*)
let intersection l1 l2 = call_intersection (set l1) (set l2) [];;

(*--------------------intersection: end ------------------*)

(*--------------union: start ---------------------------*)
(*will return union of two sets *)
let rec call_union l1 l2 = 
  match l1 with
  | [] -> l2
  | h1::t1-> 
   if member h1 l2 then 
    call_union t1 l2
   else
    call_union t1 (h1::l2)

(*Union calling function*)
let union l1 l2 =
  match l1 with 
  | [] -> set l2
  | h1::t1 -> 
  (
    match l2 with
    | [] -> set l1
    | h2::t2 -> call_union (set l1) (set l2) 
  )
(*--------------union: end ---------------------------*)
(*--------------difference: start ---------------------------*)
(*will return union of two sets *)
let rec call_difference l1 l2 res = 
  match l1 with
  | [] -> res
  | h1::t1 -> 
   if member h1 l2 then
    call_difference t1 l2 res
   else
    call_difference t1 l2 (h1::res)


(*subset calling function*)
let difference l1 l2 =
  match l1 with 
  | [] -> []
  | h1::t1 -> 
  (
    match l2 with
    | [] -> set l1
    | h2::t2 -> call_difference (set l1) (set l2) []
  )
(*--------------difference: end ---------------------------*)
(*--------------equal: start ------------------------------*)
(*will return true if two sets are equal else false*)
let rec equal_cal l1 l2 = 
  match l1 with
  | [] -> 
  (
    match l2 with 
    |[] -> true
    | h2::t2 -> false
  )
  |h1::t1 -> 
  (
    match l2 with 
    | [] -> false
    | h2::t2->
    (
      if difference l1 l2 = [] then
        if difference l2 l1 = [] then 
          true
        else
          false
      else
        false

    )
  )

(*Equal calling function*)

let equal l1 l2 = equal_cal (set l1) (set l2);;

(*--------------equal: end ---------------------------*)
(*--------------subset: start ---------------------------*)
(*It returns true if s1 is subset of s2 else false *)
let rec call_subset l1 l2 = 
  match l1 with
  | [] -> true
  | h1::t1 -> 
   if member h1 l2 then
    call_subset t1 l2
   else
    false


(*subset calling function*)
let subset l1 l2 =
  match l1 with 
  | [] -> true
  | h1::t1 -> 
  (
    match l2 with
    | [] -> false
    | h2::t2 -> call_subset (set l1) (set l2)
  )
(*--------------difference: end ---------------------------*)

(*--------------power: start ---------------------------*)
(*It returns powerset of the given set or can return all sublists of a given list *)

let rec power_cal l1 = 
  match l1 with
  |[] -> [[]]
  |h1::t1 ->
  (
    let sublist  = power_cal t1 in 
      sublist @ (List.map (fun t-> h1::t)sublist)
  )
  ;;

(*power calling function*)
let power l1 =
  match l1 with
  | [] ->[[]]
  |h1::t1 -> power_cal (set l1)
(*--------------difference: end ------------------------------*)
(*--------------cross roduct: start ---------------------------*)
(*It returns powerset of the given set or can return all sublists of a given list *)

let rec product_helper_func x l1 res = 
match l1 with 
| []-> res
| h1::t1 -> product_helper_func x t1 ((x,h1)::res)

let rec product_cal l1 l2 res= 
  match l1 with
  |[] -> res
  |h1::t1 ->
  ( 
    product_cal t1 l2 (res@(product_helper_func h1 l2 []))
  )
  ;;

(*subset calling function*)
let product l1 l2 =
  match l1 with
  | [] -> []
  |h1::t1 -> 
  (
    match l2 with
    | []-> []
    |h2::t2 -> product_cal (set l1) (set l2) []
  )
(*--------------difference: end ---------------------------*)
end

module Cs:
sig
val emptyset : unit -> bool
val member : 'a -> ('a -> 'b) -> 'b
val union : ('a -> bool) -> ('a -> bool) -> 'a -> bool
val intersection : ('a -> bool) -> ('a -> bool) -> 'a -> bool
val difference : ('a -> bool) -> ('a -> bool) -> 'a -> bool
val product : ('a -> bool) -> ('b -> bool) -> 'a * 'b -> bool
end =
struct
(*Emptyset*)
let emptyset () = false;;

(*membership*)
let member x f = f x;;

(*union*)
let union f g x = f x || g x;;

(*intersection*)
let intersection f g x = (f x) && (g x);;

(*difference*)
let difference f g x= (f x) && (not(g x));;

(*product*)
let product f g (x,y) = (f x) && (g y);;
end

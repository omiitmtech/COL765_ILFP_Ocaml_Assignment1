let fs x = match x with
    1|2|3|4|5 -> true
  | _ -> false;;


let ft x = match x with
    3|4|5|6|7 -> true
  | _ -> false;;



let evenx x = if (x mod 2 = 0) then true else false ;;
let mod3 x = if (x mod 3 = 0) then true else false ;; 

Ls.product [] [3;4];;

Ls.product [1;2] [];;

Ls.product [1;2] [3;4];;
Ls.product [1;2;4;5;6;7] [8;9;10];;

let s1 = [1;4;5;];;
let s2 = [6;7;3;4;5;];;
Ls.union s1 s2;;

let s1 = [1;4;5;8];;
let s2 = [6;7;3;4;5;4];;
Ls.difference s1 s2;;
Ls.intersection s1 s2;; 
Ls.member 3 [1;3;5;2;8];;
Ls.member [3] [[1];[3];[5];[2];[8;6;7;7]];;
Ls.union [] [];;
Ls.union [1;2;3] [];;
Ls.union [] [1;2;3];;
Ls.union [1;2;3] [4;5;6];;
Ls.union [1;2;3] [1;6;3];;
Ls.union [1;1;2] [1;2;3;3];;
Ls.difference [] [];;
Ls.difference [1;2;3] [];;
Ls.difference [] [1;2;3];;
Ls.difference [1;2;3] [4;5;6];;
Ls.difference [1;2;3] [1;6;3];;
Ls.difference [1;1;2;3] [1;2;3;3];;
Ls.difference [[1;3];[4];[2;3]] [[1;2];[2;3];[ ]];;
Ls.difference [[1;3];[4];[2;3]] [[1;2];[2;3];[4]];;
Ls.difference ["aa";"bb";"cc";"bb"] ["aa";"ee";"";"dd"];; 


Ls.equal [] [];;
Ls.equal [1;2;3] [];;
Ls.equal [] [1;2;3];;
Ls.equal [1;2;3] [4;5;6];;
Ls.equal [1;2;3] [3;2;1];;
Ls.equal [1;2;3] [1;2;3];;
Ls.equal ["aa";"bb";"cc";"bb"] ["aa";"bb";"cc"];; 
Ls.equal ["aa";"bb";"cc";"bb"] ["cc";"aa";"bb"];; 
Ls.equal [[1;2];[2;3]] [[1;2];[2;3]];;
Ls.equal [[1;2];[4];[2;3]] [[1;2];[2;3];[4]];;
  Ls.equal [2;2;3] [3;2;2]  ;; (* gives true *)

  Ls.equal [1;2;3] [3;1;2]  ;; (* gives true as oerder doesent matter *)



Ls.power [1;2;3] ;;
Ls.power [1;2;3];;
Ls.power [1;2;34;5;6];;
Ls.power [1;2;3;2;1];;
Ls.power [1;2;51;2;3];;
Ls.power ["aa";"bb";"cc";"dd"];; 
Ls.power ["aa";"bb";"cc"];; 



Ls.product [1;2] [];;
Ls.product [] [3;4];;
Ls.product [1;2] [3;4];;
Ls.product [[1];[2;3]] [[3];[4]];;
Ls.product ["aa";"bb"] ["aa";"cc"];; 


(*(Test Cases Functions as sets)*)

Cs.member 1 ft;; 
Cs.member 6 ft;; 
Cs.member 3 fs;; 
Cs.member 3 ft;; 

(Cs.union fs ft) 1;;
(Cs.union fs ft) 3;;
(Cs.union fs ft) 6;;
(Cs.union fs ft) 8;;


(Cs.intersection fs ft) 1;;
(Cs.intersection fs ft) 3;;
(Cs.intersection fs ft) 6;;
(Cs.intersection fs ft) 8;;
(Cs.difference fs ft) 1;;
(Cs.difference fs ft) 3;;
(Cs.difference fs ft) 6;;
(Cs.difference fs ft) 8;;

(Cs.product fs ft) (1,2);;
(Cs.product fs ft) (3,4);;
(Cs.product fs ft) (3,3);;
(Cs.product fs ft) (5,3);;
(Cs.product fs ft) (6,3);;
(Cs.product fs ft) (6,7);;
(Cs.product fs ft) (7,7);;

(Cs.product fs ft) (1,2);;    
(Cs.product fs ft) (3,4);;   

(Cs.product fs ft) (6,7);;
(* Test cases *)


Cs.member 9 mod3;;

(* - : bool = true *)

Cs.member  15 mod3;;

(* - : bool = true *)

Cs.member  28 mod3;;

(* - : bool = false *)

Cs.union evenx mod3 20;;

(* - : bool = true *)

Cs.union evenx   mod3 12;;

(* - : bool = true *)

Cs.union evenx mod3 25;;

(* - : bool = false *)
Cs.intersection evenx mod3 30;;

(* - : bool = true *)

Cs.intersection evenx mod3 6;;

(* - : bool = true *)

Cs.intersection evenx mod3 9;;

(* - : bool = false *)

Cs.intersection evenx mod3 20;;

(* - : bool = false *)

Cs.difference evenx mod3 6;;

(* - : bool = false *)

Cs.difference evenx mod3 20;;

(* - : bool = true *)

Cs.difference evenx mod3 11;;

(* - : bool = false *)

Cs.difference evenx mod3 30;;

(* - : bool = false *)

Cs.difference evenx mod3 9;;

(* - : bool = true *)



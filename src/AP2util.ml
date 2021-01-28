(* lists *)

let len(l) = List.length l;;

let fst(l) =
  match l with
    [] -> failwith "error fst : list is empty"
  | hd::_ -> hd
;;

let rec lst(l) =
  match l with
    [] -> failwith "error lst : list is empty"
    | hd::[] -> hd
    | _::tail -> lst(tail)
;;

let nth(l, k) = 
let rec nth1(l,k) =
  match l with
    []->  failwith "error nth : index must be in [0..len(l)-1]"
  | hd::tail -> if k = 0 then hd else nth1(tail,k-1)
in
if k < 0
then failwith "error  nth : index must be positive"
else nth1(l,k)
;;


let add_fst(l,e) = e::l ;;

let rec add_lst(l,e) =
  match l with
    [] -> [e]
  | hd::tail -> hd::add_lst(tail,e);;

let add_nth(l, e, k) =
let rec add_nth1(l, e, k) =
  match l with
    [] -> [e]
    | hd ::tail -> if k = 0 then e::l else hd::add_nth1(tail, e, k-1)
in 
if k < 0
then failwith "error add_nth : index k must be positive"
else
  if k > len(l)
  then failwith "error add_nth : index must be in [0..len(l)]"
  else add_nth1(l,e,k)
;;

let rem_fst(l) = 
  match l with
  [] -> failwith "error rem_fst : list is empty"
  | _::tail -> tail
;;

let rec rem_lst(l) =
  match l with
  [] -> failwith "error rem_lst : list is empty"
  | [x] -> []
  | x::tail -> x::rem_lst(tail)
 ;;

let rem_nth(l,k) =
let rec rem_nth1(l,k) =
  match l with
  | [] -> failwith "error rem_nth : index must be in [0..len(l)-1]"
  | hd:: tail -> if k = 0 then tail else hd::rem_nth1(tail, k-1)
in
if k < 0 
then failwith "error rem_nth :index k must be positive"
else rem_nth1(l,k)
;;

let concat(l1, l2) = l1 @ l2 ;;


(* arrays *)

let arr_len(t) = Array.length t ;;

let arr_make(n, v) = Array.make n v ;;


(* aleatoire *)

let rand_init() = Random.self_init() ;;

let rand_init_expl(n) = Random.init(n) ;;

let rand_int_0(n) = Random.int(n+1) ;;

let rand_int(n, p) = Random.int(p-n + 1) + n ;;



(* lecture controlee d un caractere *)

let read_char() : char =
  let s : string ref = ref "" and fin : bool ref = ref false in
    (
    while not(!fin) 
    do
      s:= read_line() ; 
      if String.length(!s) = 0
      then (print_string("erreur de saisie : vous n'avez saisi aucun caractère") ; print_newline() ;)
      else fin := true;
    done ;
    (!s).[0] ;
    )
;;


(* piles *)

type 'a t_pile = 'a list(* lists *)

let len(l) = List.length l;;

let fst(l) =
  match l with
    [] -> failwith "error fst : list is empty"
  | hd::_ -> hd
;;

let rec lst(l) =
  match l with
    [] -> failwith "error lst : list is empty"
    | hd::[] -> hd
    | _::tail -> lst(tail)
;;

let nth(l, k) = 
let rec nth1(l,k) =
  match l with
    []->  failwith "error nth : index must be in [0..len(l)-1]"
  | hd::tail -> if k = 0 then hd else nth1(tail,k-1)
in
if k < 0
then failwith "error  nth : index must be positive"
else nth1(l,k)
;;


let add_fst(l,e) = e::l ;;

let rec add_lst(l,e) =
  match l with
    [] -> [e]
  | hd::tail -> hd::add_lst(tail,e);;

let add_nth(l, e, k) =
let rec add_nth1(l, e, k) =
  match l with
    [] -> [e]
    | hd ::tail -> if k = 0 then e::l else hd::add_nth1(tail, e, k-1)
in 
if k < 0
then failwith "error add_nth : index k must be positive"
else
  if k > len(l)
  then failwith "error add_nth : index must be in [0..len(l)]"
  else add_nth1(l,e,k)
;;

let rem_fst(l) = 
  match l with
  [] -> failwith "error rem_fst : list is empty"
  | _::tail -> tail
;;

let rec rem_lst(l) =
  match l with
  [] -> failwith "error rem_lst : list is empty"
  | [x] -> []
  | x::tail -> x::rem_lst(tail)
 ;;

let rem_nth(l,k) =
let rec rem_nth1(l,k) =
  match l with
  | [] -> failwith "error rem_nth : index must be in [0..len(l)-1]"
  | hd:: tail -> if k = 0 then tail else hd::rem_nth1(tail, k-1)
in
if k < 0 
then failwith "error rem_nth :index k must be positive"
else rem_nth1(l,k)
;;

let concat(l1, l2) = l1 @ l2 ;;


(* arrays *)

let arr_len(t) = Array.length t ;;

let arr_make(n, v) = Array.make n v ;;


(* aleatoire *)

let rand_init() = Random.self_init() ;;

let rand_init_expl(n) = Random.init(n) ;;

let rand_int_0(n) = Random.int(n+1) ;;

let rand_int(n, p) = Random.int(p-n + 1) + n ;;



(* lecture controlee d un caractere *)

let read_char() : char =
  let s : string ref = ref "" and fin : bool ref = ref false in
    (
    while not(!fin) 
    do
      s:= read_line() ; 
      if String.length(!s) = 0
      then (print_string("erreur de saisie : vous n'avez saisi aucun caractère") ; print_newline() ;)
      else fin := true;
    done ;
    (!s).[0] ;
    )
;;


(* piles *)

type 'a t_pile = 'a list ;;

let lenp(p) = len(p) ;;
let fstp(p) = fst(p) ;;
let add_fstp(p, x) = add_fst(p, x) ;;
let rem_fstp(p) = rem_fst(p) ;;


(* files *)

type 'a t_file = 'a list ;;

let lenf(f) = len(f) ;;
let fstf(f) = fst(f) ;;
let rem_fstf(f) = rem_fst(f) ;;
let add_lstf(f, x) = add_lst(f, x) ;; ;;

let lenp(p) = len(p) ;;
let fstp(p) = fst(p) ;;
let add_fstp(p, x) = add_fst(p, x) ;;
let rem_fstp(p) = rem_fst(p) ;;


(* files *)

type 'a t_file = 'a list ;;

let lenf(f) = len(f) ;;
let fstf(f) = fst(f) ;;
let rem_fstf(f) = rem_fst(f) ;;
let add_lstf(f, x) = add_lst(f, x) ;;
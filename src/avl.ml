#directory "./";;

#load "btree.cmo";;
#load "bst.cmo";;
open Bst;;
open Btree;;


type 'a t_avltree = 'a bst;;

(*
  TODO LIST :
  - Génération arbres sous-liste de longueur variable
  - Compter le nombre de rotations effectuées (et estimer)
    + variation en fonction de la taille
    [3, 9, 12]@[1, 3, 4, 10] -> [3, 9, 12, 1, 3, 4, 10]
    [3, 2, 3, [3, 9, 12], 9, 32, [1, 3, 4, 10], 412]

    nb de rotations moyen : nb non ordonnés + nb de listes
    nb reste proportionnel au nb de noeuds non ordonnés
 *)



(*
     (Q)                (P)
    /  \               /  \
  (P)   W  --rd()->   U   (Q)
  / \                    /  \
 U   V                  V    W
 *)

let rd(avl : 'a t_avltree) : 'a t_avltree =
  if (isEmpty(avl) || isEmpty(lson(avl)))
  then invalid_arg "rd : avl and avl.lson must not be empty"
  else (
    let (p, q) = (root(lson(avl)), root(avl)) in
    let (u, v, w) = (lson(lson(avl)), rson(lson(avl)), rson(avl)) in
    rooting(p, u, rooting(q, v, w))
  )
;;



(*
    (P)                 (Q)
   /  \                /  \
  U   (Q)  --rg()->  (P)   W
     /  \           /  \
    V    W         U    V
 *)

let rg(avl : 'a t_avltree) : 'a t_avltree =
  if (isEmpty(avl) || isEmpty(rson(avl)))
  then invalid_arg "rg : avl and avl.rson must not be empty"
  else (
    let (p, q) = (root(avl), root(rson(avl))) in
    let (u, v, w) = (lson(avl), lson(rson(avl)), rson(rson(avl))) in
    rooting(q, rooting(p, u, v), w)
  )
;;


(*
       (R)                     (Q)
      /  \                   /    \
    (P)   W                (P)    (R)
   /  \       --rgd()->   /  \    /  \
  T   (Q)                T   U   V   W
     /  \
    U    V
 *)

let rgd(avl : 'a t_avltree) : 'a t_avltree =
  let (r, ls, rs) = (root(avl), lson(avl), rson(avl)) in
  let temp = rooting(r, rg(ls), rs) in
  rd(temp)  
;;


(*
    (R)                      (Q)
    /  \                    /    \
  T   (P)                (R)    (P)
      /  \   --rdg()->   /  \    /  \
    (Q)   W             T   U   V   W
  /  \
  U    V
 *)

let rdg(avl : 'a t_avltree) : 'a t_avltree =
  let (r, ls, rs) = (root(avl), lson(avl), rson(avl)) in
  let temp = rooting(r, ls, rd(rs)) in
  rg(temp) 
;;


let getValue(avl : ('a * int) t_avltree) : 'a =
  let (v, des) : ('a * int) =  root(avl) in
  v
;;

let getDes(avl : ('a * int) t_avltree) : int =
  let (v, des) : ('a * int) =  root(avl) in
  des
;;


(* Retourne la plus grande des deux valeurs d'entrée *)
let v_max(a, b : 'a * 'a) : 'a = 
  if (a >= b)
  then a
  else b
;;

(* Retourne hauteur de l'avl *)
let rec avl_height(avl : 'a t_avltree) : int = 
  if isEmpty(avl)
  then 0
  else 1 + v_max(avl_height(lson(avl)), avl_height(rson(avl)))
;;

(* 
  Retourne le déséquilibre de l'avl
  On calcule le déséquilibre de l'avl de la manière suivante :
  desequilibre = hauteur(lson) - hauteur(rson)
  avec lson, rson les fils gauche et droit de l'arbre d'entrée
 *)
let desequilibre(avl :'a t_avltree) : int = 
  if isEmpty(avl)
  then 0
  else (avl_height(lson(avl)) - avl_height(rson(avl)))
;;


(*
  Retourne l'élément maximal de l'avl
 *)

let rec max(avl : 'a t_avltree) : 'a =
  if isEmpty(rson(avl))
  then root(avl)
  else max(rson(avl))
;;

(*
  Retourne un avl privé de son éĺément maximal qu'il faut rééquilibrer.
 *)
let rec dmax(avl : 'a t_avltree) : 'a t_avltree =
  if isEmpty(avl)
  then invalid_arg "dmax : avl must not be empty"
  else (
    if isEmpty(rson(avl))
    then lson(avl)
    else rooting(root(avl), lson(avl), dmax(rson(avl)))
  )
;;

(*
  Équilibre l'avl
 *)

let reequilibrer( avl : 'a t_avltree) : 'a t_avltree =
  let des = desequilibre(avl) in
  if (des = 0 || des = -1 || des = 1)
  then avl
  else 
    if des = 2
    then 
      if desequilibre(lson(avl)) = 1
      then rd(avl)
      else rgd(avl)
    else 
      if des = -2
      then 
        if desequilibre(rson(avl)) = 1
        then rdg(avl)
        else rg(avl)
      else invalid_arg "reequilibrer: error desequilibre value"
;;

let reequilibrer_improved(avl : ('a * int) t_avltree) : ('a * int) t_avltree =
  let des = getDes(avl) in
  if (des = 0 || des = -1 || des = 1)
  then avl
  else 
    if des = 2
    then 
      if getDes(lson(avl)) = 1
      then rd(avl)
      else rgd(avl)
    else 
      if des = -2
      then 
        if getDes(rson(avl)) = 1
        then rdg(avl)
        else rg(avl)
      else invalid_arg "reequilibrer: error desequilibre value"
;;

(*
  Supprime A de l'avl  
 *)
let rec suppr_avl(a, avl : 'a* 'a t_avltree) : 'a t_avltree =
  if isEmpty(avl)
  then empty()
  else
    if a < root(avl)
    then reequilibrer( rooting( root(avl), suppr_avl( a, lson(avl) ), rson(avl)))
    else
      if a > root(avl)
      then reequilibrer( rooting( root(avl), lson(avl), suppr_avl(a, rson(avl))))
      else
        if isEmpty(rson(avl))
        then lson(avl)
        else
          if isEmpty(lson(avl))
          then rson(avl)
          else reequilibrer( rooting( max(lson(avl)), dmax(lson(avl)), rson(avl)))
;;

(*
  Ajoute A à l'avl
 *)
let rec insert_avl(a, avl : 'a * 'a t_avltree) : 'a t_avltree =
  if isEmpty(avl)
  then rooting(a, empty(), empty())
  else (
    let (r, ls, rs) = (root(avl), lson(avl), rson(avl)) in
    if a < r
    then reequilibrer( rooting( r, insert_avl( a, ls), rs))
    else 
      if a > r 
      then reequilibrer( rooting( r, ls, insert_avl( a, rs)))
      else 
        if a = r
        then avl
        else invalid_arg "insert_avl : root value error"
  )
;;

let rec bst_seek (elem, tree : 'a * 'a t_avltree) : bool =
  if isEmpty(tree)
  then false
  else
    if elem < root(tree)
    then bst_seek(elem, lson(tree))
    else
      if elem > root(tree)
      then bst_seek(elem, rson(tree))
      else true
;;


let rec avl_rnd_create_aux (l, t : int list * int t_avltree) : int t_avltree =
  match l with
  | [] -> t
  | hd::tl -> avl_rnd_create_aux(tl, insert_avl(hd, t))
;;


let avl_rnd_create (l : int list) : int t_avltree =
  let t : int t_avltree = rooting( List.hd(l), empty(), empty()) in
  avl_rnd_create_aux(List.tl(l), t)
;;

Random.self_init;;
let rec random_list_int( n, max_val : int * int ) : int list =
  if n <= 0
  then []
  else Random.int(max_val)::random_list_int(n-1, max_val)
;;


Random.self_init;;
let rec random_sublist( n, max_val,last_val : int * int * int ) : int list =
  if n <= 0
  then []
  else
    (
    let m:int = (Random.int(max_val)+last_val)in
    m::random_sublist(n-1, max_val,m)
    )
;;

let rec random_list_sub_int( n, max_val,percent : int * int*int ) : int list =
  if n <= 0
  then []
  else
    (
      let m:int = (Random.int(max_val))in
      if(m<((max_val*percent)/100))
      then
        random_sublist(5,max_val,0)@random_list_sub_int(n-1,max_val,percent)
      else
        m::random_list_sub_int(n-1,max_val,percent)
    )
;;
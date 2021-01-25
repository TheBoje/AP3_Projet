#directory "../data/";;

#load "btree.cmo";;
#load "bst.cmo";;
open Bst;;
open Btree;;


type 'a t_avltree = 'a bst;;

(*
  TODO LIST :
  | Vérifier si bst_seek fonctionne pour les avl
  - Test de complexité des algos (log n)
  - Génération arbres sous-liste de longueur variable
  - Compter le nombre de rotations effectuées (et estimer)
    + variation en fonction de la taille
 *)


(*
     (Q)                (P)
    /  \               /  \
  (P)   W  --rd()->   U   (Q)
  / \                    /  \
 U   V                  V    W
 *)

let rd(avl : 'a t_avltree) : 'a t_avltree =
  if (isEmpty(avl) || isEmpty(rson(avl)))
  then invalid_arg "rd : avl and avl.rson must not be empty"
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
  if (isEmpty(avl) || isEmpty(lson(avl)))
  then invalid_arg "rg : avl and avl.lson must not be empty"
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
  if (isEmpty(avl) || isEmpty(lson(avl)) || isEmpty(rson(lson(avl))))
  then invalid_arg "rgd : avl and avl.lson and avl.lson.rson must not be empty"
  else (
    let (r, p, q) = (root(avl), root(lson(avl)), root(rson(lson(avl)))) in
    let (t, u, v, w) = (lson(lson(avl)), lson(rson(lson(avl))), rson(rson(lson(avl))), rson(avl)) in
    rooting(q, rooting(p, t, u), rooting(r, v, w))
  )
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
  if (isEmpty(avl) || isEmpty(rson(avl)) || isEmpty(lson(rson(avl))))
  then invalid_arg "rgd : avl and avl.rson and avl.rson.lson must not be empty"
  else (
    let (r, p, q) = (root(avl), root(rson(avl)), root(lson(rson(avl)))) in
    let (t, u, v, w) = (lson(avl), lson(lson(rson(avl))), rson(lson(rson(avl))), rson(rson(avl))) in
    rooting(q, rooting(r, t, u), rooting(p, v, w))
  )
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
  else
    if (isEmpty(lson(avl)) && isEmpty(rson(avl)))
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

let rec reequilibrer( avl : 'a t_avltree) : 'a t_avltree =
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
        then rg(avl)
        else rdg(avl)
      else invalid_arg "reequilibrer: error desequilibre value"
;;

(*
  Supprime A de l'avl  
 *)
let rec suppr_avl(a,avl : 'a* 'a t_avltree) : 'a t_avltree =
  if isEmpty(avl)
  then empty()
  else
    if a < root(avl)
    then reequilibrer( rooting( root(avl), suppr_avl( a, lson(avl) ), rson(avl)))
    else
      if a > root(avl)
      then reequilibrer( rooting( root(avl), lson(avl), suppr_avl(a, rson(avl))))
      else
        if isEmpty( rson(avl) )
        then lson(avl)
        else
          if isEmpty( lson(avl) )
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

let set_elem (l, i, x : int list * int * int) : int list =
  List.mapi(fun i' el -> if i = i' then x else el) l
;;


let rec random_list_int( n : int ) : int list =
  if n <= 0
  then []
  else Random.int(20)::random_list_int(n-1)
;;





(* === TESTS === *)
let test_avl_rd : int t_avltree = 
  rooting(1, 
          rooting(2, 
                  rooting(3, empty(), empty()),
                  rooting(4, empty(), empty())
            ),
          rooting(5, empty(), empty())
    )
;;
show_int_btree(test_avl_rd);; 
show_int_btree(rd(test_avl_rd));;


let test_avl_rg : int t_avltree = 
  rooting(1, 
          rooting(3, empty(), empty()),
          rooting(2, 
                  rooting(4, empty(), empty()),
                  rooting(5, empty(), empty())
            )
    )
;;
show_int_btree(test_avl_rg);; 
show_int_btree(rg(test_avl_rg));;


let test_avl_rgd : int t_avltree = 
  rooting(6, 
          rooting(2, 
                  rooting(1, empty(), empty()),
                  rooting(4, 
                          rooting(3, empty(), empty()),
                          rooting(5, empty(), empty())
                    )
            ),
          rooting(7, empty(), empty())
    )
;;
show_int_btree(test_avl_rgd);; 
show_int_btree(rgd(test_avl_rgd));;


let test_avl_rdg : int t_avltree =
  rooting(2,
          rooting(1, empty(), empty()),
          rooting(6,
                  rooting(4,
                          rooting(3, empty(), empty()),
                          rooting(5, empty(), empty())
                    ),
                  rooting(7, empty(), empty())  
            )
    )
;;
show_int_btree(test_avl_rdg);; 
show_int_btree(rdg(test_avl_rdg));;


let test_desequilibre : int t_avltree = test_avl_rd;;
show_int_btree(test_desequilibre);;
desequilibre(test_desequilibre);;


let test_reequilibre: int t_avltree= test_avl_rdg;;
show_int_btree(test_reequilibre);;
show_int_btree(reequilibrer(test_reequilibre));;


let test_insert_avl: int t_avltree= test_avl_rdg;;
show_int_btree(test_insert_avl);;
desequilibre(test_insert_avl);;
show_int_btree(insert_avl(8,test_insert_avl));;
desequilibre((insert_avl(8,test_insert_avl)));;


let test_supr_avl : int t_avltree= insert_avl(8,test_insert_avl);;
show_int_btree(test_supr_avl);;
desequilibre(test_supr_avl);;
show_int_btree(suppr_avl(4,test_supr_avl));;
desequilibre(suppr_avl(4,test_supr_avl));;

bst_seek(5,test_supr_avl);;
bst_seek(10,test_supr_avl);;


let test_list: int list = [3;8;1;9;4;7];;
let test_create_avl: int t_avltree = avl_rnd_create(test_list);;
show_int_btree(test_create_avl);;

show_int_btree(avl_rnd_create(random_list_int(5)));;
show_int_btree(avl_rnd_create(random_list_int(6)));;

let temp = random_list_int(20);;
let temp2 = avl_rnd_create(temp);;
show_int_btree(temp2);;

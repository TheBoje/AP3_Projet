#directory "../data/";;

#load "btree.cmo";;
#load "bst.cmo";;
open Bst;;
open Btree;;

type 'a t_avltree = 'a bst;;

(* A implémenter : (- a faire, + en cours, | fait)
  | rd('a t_avltree) -> 'a t_avltree
  | rg('a t_avltree) -> 'a t_avltree
  | rdg('a t_avltree) -> 'a t_avltree
  | rgd('a t_avltree) -> 'a t_avltree
  - reequilibrer('a t_avltree) -> 'a t_avltree
  - desiquilibre('a t_avltree) -> int
  - max('a t_avltree) -> 'a
  - dmax('a t_avltree) -> 'a t_avltree
  - suppr_avl('a, 'a t_avltree) -> 'a t_avltree
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



(* === TESTS === *)
let test_avl_rd : int t_avltree = rooting(1, rooting(2, rooting(3, empty(), empty()), rooting(4, empty(), empty())), rooting(5, empty(), empty()));;
show_int_btree(test_avl_rd);; 
show_int_btree(rd(test_avl_rd));;

let test_avl_rg : int t_avltree = rooting(1, rooting(3, empty(), empty()), rooting(2, rooting(4, empty(), empty()), rooting(5, empty(), empty())));;
show_int_btree(test_avl_rg);; 
show_int_btree(rg(test_avl_rg));;

let test_avl_rgd : int t_avltree = 
  rooting(1, 
    rooting(2, 
      rooting(4, empty(), empty()),
      rooting(3, 
        rooting(5, empty(), empty()),
        rooting(6, empty(), empty())
      )
    ),
    rooting(7, empty(), empty())
  )
;;
show_int_btree(test_avl_rgd);; 
show_int_btree(rgd(test_avl_rgd));;

let test_avl_rdg : int t_avltree =
  rooting(1,
    rooting(4, empty(), empty()),
    rooting(2,
      rooting(3,
        rooting(5, empty(), empty()),
        rooting(6, empty(), empty())
      ),
      rooting(7, empty(), empty())  
    )
  )
;;
show_int_btree(test_avl_rdg);; 
show_int_btree(rdg(test_avl_rdg));;
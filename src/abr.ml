#load "btree.cmo";;
open Btree;;

type 'a bst = 'a t_btree;;


let rec bst_seek (elem, tree : 'a * 'a bst) : bool =
  if isEmpty(tree)
  then
    false
  else
    if elem < root(tree)
    then
      bst_seek(elem, lson(tree))
    else
      if elem > root(tree)
      then
        bst_seek(elem, rson(tree))
      else
        true
;;

let rec bst_linsert (elem, tree : 'a * 'a bst) : 'a bst =
 let (v, g, d) : ('a * 'a bst * 'a bst) = root(tree), lson (tree), rson(tree) in
  if (isEmpty(tree))
  then rooting(elem, empty(), empty())
  else
    if elem = v
    then tree
    else
      if elem < v
      then rooting (v, bst_linsert(elem, g), d)
      else rooting (v, d, bst_linsert(elem, g))
;;


let rec bst_lbuild (l : 'a list): 'a bst =
  match l with
  | [] -> empty()
  | v::lt -> bst_linsert(v, bst_lbuild(lt))
;;

let rec max(t : 'a bst) : 'a =
  if isEmpty(t)
  then
    invalid_arg "max : l'arbre est vide"
  else
    if isEmpty(rson(t))
    then
      root(t)
    else
      max(rson(t))
;;

let rec dmax(t : 'a t_btree) : 'a t_btree =
  if isEmpty(t)
  then invalid_arg "dmax l'arbre est vide"
  else
    if isEmpty(rson(t))
    then lson(t)
    else rooting(root(t), lson(t), dmax(rson(t)))
;;

  
let rec  bst_delete(e, t : 'a * 'a bst): 'a bst =
  if isEmpty(t)
  then t
  else
    let (v, g, d) = (root(t), lson(t), rson(t))
    in
    if e<v
    then
      rooting(v, bst_delete(e, g), d)
    else
      if e > v
      then
        rooting(v, g, bst_delete(e, d))
      else
        if isEmpty(g)
        then d
        else
          if isEmpty(d)
          then g
          else
            rooting(max(g), dmax(g), d)
;;
let l = [ 5 ; 6 ; 7 ; 1; 8; 154; 30 ; 58];;
bst_lbuild(l);;

let rec gen_rnd_lst (size, l : int * int list) : int list =
  if size = 0
  then l
  else
    let n = Random.int 500 in
    let list = n::l in
    gen_rnd_lst(size - 1, list)
;;

let  bst_rnd_create (size : int) : int bst =
  let l = gen_rnd_lst(size, []) in
  bst_lbuild(l)
;;

            


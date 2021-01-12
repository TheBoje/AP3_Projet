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
  if (isEmpty(tree))
  then rooting(elem, empty(), empty())
  else
    let (v, g, d) : ('a * 'a bst * 'a bst) = root(tree), lson (tree), rson(tree) in
    if elem = v
    then tree
    else
      if elem < v
      then rooting (v, bst_linsert(elem, g), d)
      else rooting (v, g, bst_linsert(elem, d))
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

let max (a , b : int * int ) = Pervasives.max a b;;

let rec height (tree : 'a t_btree) : int =
  if (tree = empty() || rson(tree) = empty() && lson(tree) = empty())
  then 0
  else 1 + max(height(rson(tree)), height(lson(tree)))
;;


let unbalance (tree : int bst) : int =
  if isEmpty(tree)
  then 0
  else
    let (v, g, d) : ('a * 'a bst * 'a bst) = root(tree), lson (tree), rson(tree) in
    height(g) - height(d)
;;

let unbalanceAvg (tsample, treesSize : int * int) : float =

  let sum : float ref = ref 0. in

  for i=1 to tsample
  do
    sum := !sum +. float_of_int(unbalance(bst_rnd_create(100)))
  done;

  !sum /. float_of_int(tsample)
;;

let unbalanceAvgsAvg(avgSample, treeSample, treesSize : int * int * int) : float =
  
  let sum : float ref = ref 0. in

  for i=1 to avgSample
  do
    sum := !sum +. unbalanceAvg(treeSample, treesSize);
  done;

  !sum /. float_of_int(avgSample)
;;





    
let b : int bst  = rooting(10, empty(), empty);;
bst_linsert(4,b );;      

let t = bst_rnd_create(50);;

unbalance(t);;
unbalanceAvg(250, 100);;
unbalanceAvgsAvg(1000, 250, 100);;

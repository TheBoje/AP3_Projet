#load "btree.cmo";;
#load "bst.cmo";;
#use "AP2util.ml";;
#load "graphics.cma";;
#use "graphics.ml";;

open Btree;;
open List;;
type 'a bst = 'a t_btree;;

(********* FONCTIONS DU MODULE FAIT EN TP ************)
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

let rec max_seek(t : 'a bst) : 'a =
  if isEmpty(t)
  then
    invalid_arg "max : l'arbre est vide"
  else
    if isEmpty(rson(t))
    then
      root(t)
    else
      max_seek(rson(t))
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
            rooting(max_seek(g), dmax(g), d)
;;

let max (a , b : int * int ) = Pervasives.max a b;;

let rec height (tree : 'a t_btree) : int =
  if (tree = empty() || rson(tree) = empty() && lson(tree) = empty())
  then 0
  else 1 + max(height(rson(tree)), height(lson(tree)))
;;

let rec size( tree : 'a t_btree) : int =
  if (tree = empty())
  then 0
  else 1 + size(rson(tree)) + size(lson(tree))
;;

(********** FONCTIONS UTILITAIRES AU PROJET ************)     

(******** Fonctions de génération de liste ********)

(* Génère une liste de size nombres aléatoires *)
let rec gen_rnd_lst_aux (size, l : int * int list) : int list =
  if size = 0
  then l
  else
    let n = Random.int 500 in
    let list = n::l in
    gen_rnd_lst_aux(size - 1, list)
;;

let gen_rnd_lst(size : int ) : int list =
  gen_rnd_lst_aux(size, [])
;;

(* Génère une liste contenant une suite de 1 à size *)
let rec gen_seq_lst_aux(borneMin, borneMax , list : int * int * int list) : int list =
  if borneMax = borneMin
  then list
  else
    gen_seq_lst_aux(borneMin, borneMax - 1, borneMax::list)
;;


let gen_seq_lst( borneMin, borneMax :int * int) : int list =
  gen_seq_lst_aux(borneMin,borneMax, [])
;;

(* Génère une liste contenant des suites ordonnée et des suites non-ordonnées *)


let rec gen_mixed_lst_aux( size, l : int *  int list) : int list =
  if size <= 0
  then l
  else
    if size mod 2 = 0 && size != 2
    then
      let rndListLength : int = size/4 in
      let rndList : int list = gen_rnd_lst(rndListLength) in
      gen_mixed_lst_aux(size - rndListLength, rndList@l)
    else
      let min : int = Random.int size in
      let seqList : int list = gen_seq_lst(min, size) in
      gen_mixed_lst_aux(size - length(seqList) , seqList@l)
;;

let gen_mixed_lst (size : int ) : int list =
  gen_mixed_lst_aux(size, [])
;;

(******** Fonctions de génération d'arbre ********)

(*Génère un ABR à partir d'une liste de nombre aléatoire de taille size*)
let  bst_rnd_create (size : int) : int bst =
  let l = gen_rnd_lst(size) in
  bst_lbuild(l)
;;

(* Génère un ABR à partir d'une liste ordonnée *)
let bst_seq_create(size : int) : int bst =
  let min = Random.int size in
  let l : int list = gen_seq_lst(min, size) in
   bst_lbuild(l)
;;

let bst_mix_create(size : int) : int bst =
  let l : int list = gen_mixed_lst(size) in
  bst_lbuild(l)
;;

(******** Fonctions de calcul ********)

(* Retourne le déséquilibre entre le fils droit et le fils gauche d'un arbre
soit ; la différence de hauteur entre le fils gauche et le fils droit *)
let unbalance (tree : int bst) : int =
  if isEmpty(tree)
  then 0
  else
    let (v, g, d) : ('a * 'a bst * 'a bst) = root(tree), lson (tree), rson(tree) in
    height(g) - height(d)
;;

(* Retourne la moyenne de déséquilibre calculés sur tsample abr aléatoires de taille treesSize *)
let rnd_unbalance_avg (tSample, treesSize : int * int) : float =

  let sum : float ref = ref 0. in

  for i=1 to tSample
  do
    sum := !sum +. float_of_int(unbalance(bst_rnd_create(treesSize)))
  done;

  !sum /. float_of_int(tSample)
;;

(* Retourne la moyenne de avgSample déséquilibres *)
let rnd_unbalance_avgs_avg(avgSample, treeSample, treesSize : int * int * int) : float =
  
  let sum : float ref = ref 0. in

  for i=1 to avgSample
  do
    sum := !sum +. rnd_unbalance_avg(treeSample, treesSize);
  done;

  !sum /. float_of_int(avgSample)
;;


(* unbalance_avg mais avec des arbres construits à partir d'une liste composée de sous-suites *)
let mixed_unbalance_avg (tSample, treesSize : int * int) : float =
  
  let sum : float ref = ref 0. in

  for i=1 to tSample
  do
    sum := !sum +. float_of_int(unbalance(bst_mix_create(treesSize)))
  done;

  !sum /. float_of_int(tSample)
;;

let mixed_unbalance_avgs_avg (avgSample, treeSample, treesSize : int * int * int) : float =
  
  let sum : float ref = ref 0. in

  for i=1 to avgSample
  do
    sum := !sum +. mixed_unbalance_avg(treeSample, treesSize);
  done;

  !sum /. float_of_int(avgSample)
;;

(******** TESTS ********)

 let rnd_avg_compute(n : int) : float array * float array =
  let indices : float array = arr_make(n + 1, 0.0) in
  let temps : float array = arr_make(n + 1, 0.0) in
  (
    for i = 1 to n 
    do 
      temps.(i) <- Sys.time();
      ignore(rnd_unbalance_avgs_avg(100,100,i));
      temps.(i) <- Sys.time() -. temps.(i);
      indices.(i) <- float_of_int(i);
      done;
      (temps, indices);
  )
 ;;
 
 
let rnd_avg_plot(n : int) : float =
  let init_time : float = Sys.time() in
  let (temps, indices) : float array * float array = rnd_avg_compute(n) in
  let repere : t_rep = {orx = 50; ory = 50; extx = 900; exty = 500} in
  (
    open_graph(1000, 600);
    clear_graph();
    draw_rep(repere);
    draw_curve(temps, indices, arr_len(indices) - 1, repere);
    Sys.time() -. init_time;
    );;

(*rnd_avg_plot(100);;*)


let mixed_compute(n : int) : float array * float array =
  let indices : float array = arr_make(n + 1, 0.0) in
  let temps : float array = arr_make(n + 1, 0.0) in
  (
    for i = 1 to n 
    do 
      temps.(i) <- Sys.time();
      ignore(mixed_unbalance_avgs_avg(100,i,100));
      temps.(i) <- Sys.time() -. temps.(i);
      indices.(i) <- float_of_int(i);
      done;
      (temps, indices);
      )
;;

let mixed_avg_plot(n : int) : float =
  let init_time : float = Sys.time() in
  let (temps, indices) : float array * float array = mixed_compute(n) in
  let repere : t_rep = {orx = 50; ory = 50; extx = 900; exty = 500} in
  (
    open_graph(1000, 600);
    clear_graph();
    draw_rep(repere);
    draw_curve(temps, indices, arr_len(indices) - 1, repere);
    Sys.time() -. init_time;
    )
;;

(*mixed_avg_plot(100);;*)

(*
rnd_unbalance_avg(100, 100);;
mixed_unbalance_avg(100, 100);;

rnd_unbalance_avgs_avg(1000, 100, 100);;
mixed_unbalance_avgs_avg(1000, 100, 100);;*)

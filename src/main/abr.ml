(* ================================================== *)
(* ================== MODULE ABR.ML ================= *)
(*
  Ce module a été créé par le groupe d'étudiants suivant :
  - Yann Berthelot
  - Louis Leenart
  - Alexis Louail
  Le contenu de ce module a été fait d'apres le sujet de
  projet d'Algorithmique et Programmation 3 en suivant 
  les axiomes décrits en cours.
*)
(* ================================================== *)


(* ================================================== *)
(* ================== IMPORTATIONS ================== *)
(* ================================================== *)

#directory "../usage";;

#load "btree.cmo";;
#load "bst.cmo";;
#load "graphics.cma";;
#use "ap2util.ml";;
#use "graphics.ml";;

open Btree;;
open List;;

(* ================================================== *)
(* ====================== TYPE ====================== *)
(* ================================================== *)

type 'a bst = 'a t_btree;;

(* ================================================== *)
(* ================ FONCTION ABR TP ================= *)
(* ================================================== *)


Random.self_init;;


(*
  Recherche une valeur dans l'arbre
  input :
  - elem : valeur du noeud à trouver
  - tree : arbre dans lequel on cherche elem
  output : 
  - bool : TRUE - elem existe dans tree
          FALSE - elem n'existe pas dans tree
 *)
let rec bst_seek (elem, tree : 'a * 'a bst) : bool =
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


(*
  Ajout d'un élément dans l'arbre
  input : 
  - elem : valeur à ajouter
  - tree : arbre auquel on souhaite ajouter elem
  output :
  - 'a bst : tree auquel on a ajouté elem
  Note: si elem existe déjà dans tree, alors
  elem n'est pas ajouté et tree n'est pas modifié.
 *)
let rec bst_linsert (elem, tree : 'a * 'a bst) : 'a bst =
  if (isEmpty(tree))
  then rooting(elem, empty(), empty())
  else
    let (v, g, d) :
      ('a * 'a bst * 'a bst) = 
      root(tree), 
      lson (tree), 
      rson(tree) in

    if elem = v
    then tree
    else
      if elem < v
      then rooting(
            v, 
            bst_linsert(elem, g), 
            d
          )
      else rooting(
            v, 
            g, 
            bst_linsert(elem, d)
          )
;;


(*
  Construction d'un bst à partir d'une liste
  input : 
  - l : liste de valeurs
  output :
  - 'a bst : arbre contenant les valeurs de la liste
 *)
let rec bst_lbuild (l : 'a list): 'a bst =
  match l with
  | [] -> empty()
  | v::lt -> bst_linsert(v, bst_lbuild(lt))
;;


(*
  Cherche l'élément maximum de l'arbre
  input : 
  - tree : arbre pour lequel on cherche le max
  output :
  - 'a : élément max de tree
 *)
let rec max_seek(tree : 'a bst) : 'a =
  if isEmpty(tree)
  then invalid_arg "max : l'arbre est vide"
  else
    if isEmpty(rson(tree))
    then root(tree)
    else max_seek(rson(tree))
;;


(*
  Retire l'élément maximal de l'arbre
  input : 
  - tree : arbre dont on souhaite retirer l'élément maximal
  output :
  - 'a t_btree : arbre dont on a retiré l'élément maximal
 *)
let rec dmax(tree : 'a t_btree) : 'a t_btree =
  if isEmpty(tree)
  then invalid_arg "dmax l'arbre est vide"
  else
    if isEmpty(rson(tree))
    then lson(tree)
    else rooting(root(tree), lson(tree), dmax(rson(tree)))
;;


(*
  Supprime un noeud de l'arbre
  input : 
  - elem : valeur du noeud à supprimer
  - tree : arbre auquel on souhaite retirer elem
  output :
  - 'a bst : arbre auquel on a suprimé le noeud
 *)
let rec bst_delete(elem, tree : 'a * 'a bst): 'a bst =
  if isEmpty(tree)
  then tree
  else
    let (v, ls, rs) = (root(tree), lson(tree), rson(tree))
    in
    if elem < v
    then rooting(v, bst_delete(elem, ls), rs)
    else
      if elem > v
      then rooting(v, ls, bst_delete(elem, rs))
      else
        if isEmpty(ls)
        then rs
        else
          if isEmpty(rs)
          then ls
          else rooting(max_seek(ls), dmax(ls), rs)
;;


(*
  Calcule le max de deux éléments
  input : 
  - a : élément 1 à comparer
  - b : élément 2 à comparer
  output :
  - 'a : élément le plus grand entre a et b
 *)
let max (a , b : int * int ) = 
  if a > b
  then a
  else b
;;


(*
  Calcule la hauteur de la racine de l'arbre
  input : 
  - tree : arbre dont on souhaite calculer la hauteur
  output :
  - int : hauteur de la racine de l'arbre
 *)
let rec height (tree : 'a t_btree) : int =
  if (tree = empty() || rson(tree) = empty() && lson(tree) = empty())
  then 0
  else 1 + max(height(rson(tree)), height(lson(tree)))
;;


(*
  Calcule la taille de l'arbre
  input : 
  - tree : arbre dont on souhaite calculer la hauteur
  output :
  - int : hauteur de l'arbre
 *)
let rec size( tree : 'a t_btree) : int =
  if (tree = empty())
  then 0
  else 1 + size(rson(tree)) + size(lson(tree))
;;

(* ================================================== *)
(* ================ UTILITAIRES ABR ================= *)
(* ============== GÉNÉRATION DE LISTE =============== *)
(* ================================================== *)


(* Génère une liste de size nombres aléatoires *)
(*
  [FONCTION PRIVÉE]
  Génère une liste d'entiers aléatoire 
  input : 
  - size : taille de la liste à générer
  - l : liste initiale, paramètre privé
  output :
  - int list : liste d'entiers aléatoire de taille
  size, avec tout élément compris entre 0 et max_val.
 *)
let max_val : int = 3000;;
let rec _gen_rnd_lst_aux(size, l : int * int list) : int list =
  if size = 0
  then l
  else
    let n = Random.int max_val in
    let list = n::l in
    _gen_rnd_lst_aux(size - 1, list)
;;


(*
  Génère une liste d'entiers aléatoire
  input : 
  - size : taille de la liste à générer
  output :
  - int list : liste d'entiers aléatoire de taille
  size, avec tout élément compris entre 0 et max_val.
 *)
let gen_rnd_lst(size : int ) : int list =
  _gen_rnd_lst_aux(size, [])
;;


(*
  [FONCTION PRIVÉE]
  Génère une liste contenant une suite d'entiers 
  input : 
  - borneMin : TODO
  - borneMax : TODO
  - l : liste de retour, initialisée à []
  output :
  - int list : suite d'entiers de taille borneMax - borneMin
 *)
let rec _gen_seq_lst_aux(borneMin, borneMax , l : int * int * int list) : int list =
  if borneMax = borneMin
  then l
  else
    _gen_seq_lst_aux(borneMin, borneMax - 1, borneMax::list)
;;


(*
  Génère une liste contenant une suite d'entiers
  input : 
  - borneMin : TODO
  - borneMax : TODO
  output :
  - int list : suite d'entiers de taille borneMax - borneMin
 *)
let gen_seq_lst(borneMin, borneMax : int * int) : int list =
  _gen_seq_lst_aux(borneMin,borneMax, [])
;;


(*
  [FONCTION PRIVÉE]
  Génère une liste contenant des suites ordonnées et des suites
  non-ordonnées
  input : 
  - size : taille de la liste
  - l : liste de retour, initialisée à []
  output :
  - int list : liste d'entiers contenant des suites ordonnées et des
  suites non-ordonnées
 *)
let rec _gen_mixed_lst_aux(size, l : int *  int list) : int list =
  if size <= 0
  then l
  else
    if size mod 2 = 0
    then
      let rndListLength : int = (Random.int size) + 1 in
      (* +1 afin de ne pas boucler si on tombe sur Random.int size = 0 *)
      
      let rndList : int list = gen_rnd_lst(rndListLength) in
      _gen_mixed_lst_aux(size - rndListLength, rndList@l)
    else
      let min : int = Random.int size in
      let seqList : int list = gen_seq_lst(min, size) in
      _gen_mixed_lst_aux(size - length(seqList) , seqList@l)
;;


(*
  Génère une liste contenant des suites ordonnées et des suites
  non-ordonnées
  input : 
  - size : taille de la liste
  output :
  - int list : liste d'entiers contenant des suites ordonnées et des
  suites non-ordonnées
 *)
let gen_mixed_lst (size : int ) : int list =
  _gen_mixed_lst_aux(size, [])
;;


(* ================================================== *)
(* ================ GÉNÉRATION ABR ================== *)
(* ================================================== *)


(*
  Génère un ABR à partir d'une liste de nombre aléatoire
  de taille size
  input : 
  - size : taille de l'arbre à générer
  output :
  - 'a bst : arbre de taille size créé à partir d'une 
  liste d'entiers aléatoires
 *)
let  bst_rnd_create (size : int) : 'a bst =
  Random.self_init();
  let l = gen_rnd_lst(size) in
  bst_lbuild(l)
;;


(*
  Génère un ABR aléatoire contenant des sous-suites
  input : 
  - size : taille de l'ABR
  output :
  - int bst : arbre aléatoire de taille size contenant
  des sous-suites
 *)
let bst_mix_create(size : int) : int bst =
  let l : int list = gen_mixed_lst(size) in
  bst_lbuild(l)
;;


(* ================================================== *)
(* =============== FONCTIONS CALCUL ================= *)
(* ================================================== *)


(*
  Calcule le déséquilibre de la racine de l'arbre
  input : 
  - tree : arbre dont on souhaite connaitre le déséquilibre
  output :
  - int : déséquilibre de l'arbre au niveau de la racine
  Note: le calcul du déséquilibre correspond à la différence de hauteur
  entre le fils gauche et le fils droit.
 *)
let unbalance (tree : 'a bst) : int =
  if isEmpty(tree)
  then 0
  else
    let (v, g, d) : ('a * 'a bst * 'a bst) = root(tree), lson (tree), rson(tree) in
    height(g) - height(d)
;;


(*
  Calcule la moyenne de déséquilibre sur un nombre de tSample 
  ABR aléatoires (chaque ABR étant de taille treesSize)
  input : 
  - tSample : nombre d'ABR aléatoire générés pour calculer 
  la moyenne
  - treesSize : taille de chaque ABR 
  output :
  - float : moyenne de déséquilibre des tSample ABR de taille
  treesSize
 *)
let rnd_unbalance_avg (tSample, treesSize : int * int) : float =
  let sum : float ref = ref 0. in
  for i=1 to tSample
  do
    sum := !sum +. float_of_int(unbalance(bst_rnd_create(treesSize)))
  done;
  !sum /. float_of_int(tSample)
;;



(* TODO Clarifier la formulation
  Calcule la moyenne de avgSample déséquilibre
  input : 
  - avgSample : TODO
  - treeSample : TODO
  - treesSize : taille de chaque ABR
  output :
  - float : TODO
 *)
let rnd_unbalance_avgs_avg(avgSample, treeSample, treesSize : int * int * int) : float =
  let sum : float ref = ref 0. in
  for i=1 to avgSample
  do
    sum := !sum +. rnd_unbalance_avg(treeSample, treesSize);
  done;
  !sum /. float_of_int(avgSample)
;;


(*
  Calcule la moyenne de déséquilibre sur un nombre de tSample 
  ABR aléatoires (chaque ABR étant de taille treesSize) construits 
  à partir d'une liste composée de sous-suites.
  input : 
  - tSample : nombre d'ABR aléatoire générés pour calculer 
  la moyenne
  - treesSize : taille de chaque ABR 
  output :
   - float : moyenne de déséquilibre des tSample ABR de taille
  treesSize 
 *)
let mixed_unbalance_avg (tSample, treesSize : int * int) : float =
  let sum : float ref = ref 0. in
  for i=1 to tSample
  do
    sum := !sum +. float_of_int(unbalance(bst_mix_create(treesSize)))
  done;
  !sum /. float_of_int(tSample)
;;

(* TODO Aussi clarifier la formulation
  Calcule la moyenne de avgSample déséquilibre pour
  des arbres générés à partir d'une liste composée
  de sous-suites.
  input : 
  - avgSample : TODO
  - treeSample : TODO
  - treesSize : taille de chaque ABR
  output :
  - float : TODO
 *)
let mixed_unbalance_avgs_avg (avgSample, treeSample, treesSize : int * int * int) : float =
  let sum : float ref = ref 0. in
  for i=1 to avgSample
  do
    sum := !sum +. mixed_unbalance_avg(treeSample, treesSize);
  done;
  !sum /. float_of_int(avgSample)
;;

(* ================================================== *)
(* ===================== TESTS ====================== *)
(* ================================================== *)


(*
rnd_unbalance_avg(100, 100);;
mixed_unbalance_avg(100, 100);;

rnd_unbalance_avgs_avg(1000, 100, 100);;
mixed_unbalance_avgs_avg(100, 10, 100);;
*)

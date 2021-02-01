(* ================================================== *)
(* ================== MODULE AVL.ML ================= *)
(*
  Ce module a été créé par le groupe d'étudiants suivant :
  - Yann Berthelot
  - Louis Leenart
  - Alexis Louail
  Le contenu de ce module a été fait d'apres le sujet de
  projet d'Algorithmique et Programmation 3.
*)
(* ================================================== *)


(* ================================================== *)
(* ================== IMPORTATIONS ================== *)
(* ================================================== *)

#directory "./";;

#load "btree.cmo";;
#load "bst.cmo";;
open Bst;;
open Btree;;

(* ================================================== *)
(* ====================== TYPE ====================== *)
(* ================================================== *)


(* structure AVL : stockage de la valeur et de la hauteur dans le noeud via la structure suivante *)
type 'a t_avltree = ('a * int) bst;;


(* ================================================== *)
(* ================== AFFICHAGE AVL ================= *)
(* ================================================== *)


(* 
  Fonctions d'affichage des AVL et de leurs différentes 
  données. Permet d'afficher :
  - les valeurs de l'avl
  - la hauteur de chaque noeud de l'avl
  - le déséquilibre de chaque noeud de l'avl
*)

let rec _avl_to_btree(avl : 'a t_avltree) : 'a t_btree =
  if (isEmpty(avl))
  then empty()
  else rooting(getValue(avl), avl_to_btree(lson(avl)), avl_to_btree(rson(avl)))
;;

let rec _avl_to_height_btree(avl : 'a t_avltree) : int t_btree =
  if (isEmpty(avl))
  then empty()
  else rooting(getHeight(avl), avl_to_height_btree(lson(avl)), avl_to_height_btree(rson(avl)))
;;

let rec _avl_to_deseq_btree(avl : 'a t_avltree) : int t_btree =
  if (isEmpty(avl))
  then empty()
  else rooting(desequilibre(avl), avl_to_deseq_btree(lson(avl)), avl_to_deseq_btree(rson(avl)))
;;

let show_avl_tree(avl : int t_avltree) : unit =
  show_int_btree(_avl_to_btree(avl))
;;

let show_height_tree(avl : 'a t_avltree) : unit = 
  show_int_btree(_avl_to_height_btree(avl))
;;

let show_deseq_btree(avl : 'a t_avltree) : unit =
  show_int_btree(_avl_to_deseq_btree(avl))
;;

(* ================================================== *)
(* ================ UTILITAIRES AVL ================= *)
(* ================================================== *)

(*
  Récupération de la valeur du déséquilibre de l'avl
  input : 
  - avl : arbre dont on souhaite avoir le déséquilibre
  output :
  - int : valeur du déséquilibre de l'arbre
 *)
 let getHeight(avl : 'a t_avltree) : int =
  if (isEmpty(avl))
  then 0
  else
    let (value, height) : ('a * int) =  root(avl) in
    height
;;

(*
  Récupération de la valeur du noeud de l'avl
  input : 
  - avl : arbre dont on souhaite avoir la valeur au noeud
  output :
  - 'a : valeur du noeud de l'arbre
 *)
let getValue(avl : 'a t_avltree) : 'a =
  if (isEmpty(avl))
  then invalid_arg "avl: getValue on empty is not allowed"
  else
    let (value, height) : ('a * int) =  root(avl) in
    value
;;


(*
  Calcule l'élément maximal de l'arbre
  input : 
  - avl : arbre dans lequel on cherche le plus grand élément
  output : 
  'a : élément maximal de l'arbre
 *)
 let rec max(avl : 'a t_avltree) : 'a =
  if isEmpty(rson(avl))
  then getValue(avl)
  else max(rson(avl))
;;


(* 
  Calcule le max de deux éléments
  input : 
  - a : élément 1 à comparer
  - b : élément 2 à comparer
  ouput : 
  - 'a : élément le plus grand entre a et b 
*)
let max2(a, b : 'a * 'a) : 'a = 
  if a > b
  then a 
  else b
;;


(* 
  Mise à jour de la hauteur du noeud. À utiliser apres modification de l'avl
  input : 
  avl : arbre avl dont on veut mettre à jour la valeur de la hauteur dans le noeud
  output :
  'a t_avltree : avl avec valeur de hauteur mises à jour
*)
let updateHeight(avl : 'a t_avltree) : 'a t_avltree = 
  if (isEmpty(avl))
  then avl
  else 
    (
      let ((value, height), ls, rs) : (('a * int) * 'a t_avltree * 'a t_avltree) = 
        (
          root(avl),
          lson(avl),
          rson(avl)
        ) in 
      let newHeight : int = 1 + max2(getHeight(ls), getHeight(rs)) in
      rooting((value, newHeight), ls, rs)
    )
;;


(*
  Calcule le déséquilibre de l'arbre
  input : 
  - avl : arbre auquel on souhaite obtenir le déséquilibre
  output :
  - int : déséquilibre de l'arbre
  Pour calculer ce déséquilibre, on utilise la formule suivante : 
  desequilibre = hauteur(lson) - hauteur(rson) avec lson et rson 
  étant respectivement les fils gauche et droit de l'arbre.
 *)
 let desequilibre(avl :'a t_avltree) : int = 
  if isEmpty(avl)
  then 0
  else getHeight(lson(avl)) - getHeight(rson(avl))
;;


(* ================================================== *)
(* ================= ROTATIONS AVL ================== *)
(* ================================================== *)

(*
  Rotation droite de l'arbre 
  input : 
  - avl : arbre qui va subir la rotation
  output :
  - 'a t_avltree : arbre qui a subi la rotation
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
    let (u, v, w) = (
        lson(lson(avl)),
        rson(lson(avl)), 
        rson(avl)) in
    updateHeight(rooting(p, u, updateHeight(rooting(q, v, w))))
  )
;;


(*
  Rotation gauche de l'arbre 
  input : 
  - avl : arbre qui va subir la rotation
  output :
  - 'a t_avltree : arbre qui a subi la rotation
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
    let (u, v, w) = (
        lson(avl),
        lson(rson(avl)),
        rson(rson(avl))) in
    updateHeight(rooting(q, updateHeight(rooting(p, u, v)), w))
  )
;;


(*
  Rotation "gauche droite" de l'arbre 
  input : 
  - avl : arbre qui va subir la rotation
  output :
  - 'a t_avltree : arbre qui a subi la rotation
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
  rd(rooting(r, rg(ls), rs))  
;;


(*
  Rotation droite gauche de l'arbre 
  input : 
  - avl : arbre qui va subir la rotation
  output :
  - 'a t_avltree : arbre qui a subi la rotation
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
  rg(rooting(r, ls, rd(rs))) 
;;

(* ================================================== *)
(* ================= OPERATIONS AVL ================= *)
(* ================================================== *)

(*
  Rééquilibre l'arbre
  input : 
  - avl : arbre à rééquilibrer
  output :
  - 'a t_avltree : arbre rééquilibré
  Note: avec notre structure de données actuelle, la compléxité
  est de l'ordre de O(log n), avec n la taille de avl
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


(*
  Retire l'élément maximal de l'arbre
  input : 
  - avl : arbre dont on souhaite retirer l'élément maximal
  output : 
  - 'a t_avltree : arbre dont on a retiré l'élément maximal
 *)
 let rec dmax(avl : 'a t_avltree) : 'a t_avltree =
  if isEmpty(avl)
  then invalid_arg "dmax : avl must not be empty"
  else (
    if isEmpty(rson(avl))
    then lson(avl)
    else reequilibrer(updateHeight(rooting(root(avl), lson(avl), dmax(rson(avl)))))
  )
;;


(*
  Supprime un noeud de l'arbre
  input : 
  - a : valeur du noeud à supprimer
  - avl : arbre auquel on souhaite retirer a
  output : 
  - 'a t_avltree : arbre auquel on a supprimé le noeud
  Note: étant donné que cette fonction dépend de la fonction
  reequilibrer() (qui a une complexité de O(log n)), la 
  complexité de cette fonction est aussi de O(log n).
 *)
 let rec suppr_avl(a, avl : 'a* 'a t_avltree) : 'a t_avltree =
  if isEmpty(avl)
  then empty()
  else
    let ((value, height), ls, rs) : (('a * int) * 'a t_avltree * 'a t_avltree) =
      (
        root(avl),
        lson(avl),
        rson(avl)
      ) in
    if a < value
    then reequilibrer(updateHeight(rooting((value, height), suppr_avl( a, ls), rs)))
    else
      if a > value
      then reequilibrer(updateHeight(rooting((value, height), ls, suppr_avl(a, rs))))
      else
        if isEmpty(rs)
        then ls
        else
          if isEmpty(ls)
          then rs
          else reequilibrer(updateHeight(rooting((max(ls), height), dmax(ls), rs)))
;;


(*
  Ajoute un noeud a l'arbre
  input : 
  - a : valeur du noeud à ajouter
  - avl : arbre auquel on souhaite ajouter a
  output : 
  - 'a t_avltree : arbre auquel on a ajouté le noeud
  Note1: Si a existe déjà dans l'arbre avl, on ignore
  l'ajout de la valeur.
  Note2: étant donné que cette fonction dépend de la 
  fonction reequilibrer() (qui a une complexité de 
  O(log n)), la complexité de cette fonction est aussi de
  O(log n).
 *)
let rec insert_avl(a, avl : 'a * 'a t_avltree) : 'a t_avltree =
  if isEmpty(avl)
  then rooting((a, 1), empty(), empty())
  else (
    let ((value, height), ls, rs) = (root(avl), lson(avl), rson(avl)) in
    if a < value
    then reequilibrer(updateHeight(rooting((value, height), insert_avl(a, ls), rs)))
    else
      if a > value
      then reequilibrer(updateHeight(rooting((value, height), ls, insert_avl(a, rs))))
      else avl (* cas a = value*)
  )
;;

(* 
  Recherche une valeur dans l'arbre
  input : 
  - a : valeur du noeud à trouver
  - avl : arbre dans lequel on cherche a
  output : 
  - 'a t_avltree : avl contenant la valeur recherchée dans le noeud.
  Si l'élément d'existe pas dans l'avl, l'avl retourné est vide.
 *)
let rec seek_avl (elem, avl : 'a * 'a t_avltree) : 'a t_avltree =
  if isEmpty(avl)
  then avl
  else
    let ((value, height), ls, rs) = 
      (
        root(avl),
        lson(avl),
        rson(avl)
      ) in
    if elem = value
    then avl
    else
      if elem > value
      then seek_avl(elem, rs)
      else seek_avl(elem, ls)
;;


(* ================================================== *)
(* ================= GENERATION AVL ================= *)
(* ================================================== *)

(* 
  [FONCTION PRIVÉE] 
  Créé un avl à partir d'une liste
  input : 
  - l : liste des valeurs de l'avl 
  - t : arbre de retour
  output : 
  - 'a t_avltree : avl créé à partir de la liste
  d'entrée l
 *)
 let rec __avl_rnd_create_aux (l, t : 'a list * 'a t_avltree) : 'a t_avltree =
  match l with
  | [] -> t
  | hd::tl -> __avl_rnd_create_aux(tl, insert_avl(hd, t))
;;


(* 
  Crée un avl à partir d'une liste
  input : 
  - l : liste de valeur de l'arbre à créer
  output :
  - 'a t_avltree : avl créé à partir de la 
  liste l
 *)
let avl_rnd_create (l : 'a list) : 'a t_avltree =
  let t : int t_avltree = rooting( (List.hd(l), 0), empty(), empty()) in
  __avl_rnd_create_aux(List.tl(l), t)
;;


(* Initialisation nécessaire du module d'aléatoire *)
Random.self_init;;

(*
  Génère une liste d'entier de taille n avec tout élément
  compris entre 0 et max_val.
  input : 
  - n : taille de la liste d'entiers 
  - max_val : valeur maximale des élément de la liste
  output : 
  - int list : liste d'entier de n éléments compris entre
  0 et max_val.
 *)
let rec rnd_list_int(n, max_val : int * int ) : int list =
  if n <= 0
  then []
  else Random.int(max_val)::rnd_list_int(n-1, max_val)
;;

(*
  [FONCTION PRIVÉE]
  Génère une liste d'entier ordonnée de taille n avec tout
  élément compris entre last_val et max_val.
  input : 
  - n : taille de la liste d'entiers 
  - max_val : valeur maximale des élément de la liste
  - last_val : valeur minimale de la liste
  output : 
  - int list : liste d'entier de n éléments compris entre
  0 et max_val.
  Note: cette fonction est utilisée pour créer des listes 
  contenant des sous-listes ordonnées.
 *)
let rec _rnd_sublist(n, max_val, last_val : int * int * int ) : int list =
  if n <= 0
  then []
  else
    (
      let m : int = (Random.int(max_val) + last_val)in
      m::_rnd_sublist(n - 1, max_val, m)
    )
;;


(* 
  Génère une liste d'entier de taille n contenant des sous-listes
  ordonnées avec tout élément compris entre 0 et max_val.
  input : 
  - n : taille de la liste d'entiers 
  - max_val : valeur maximale des élément de la liste
  - percent : [0-100] taux d'apparition des sous-listes ordonnées
  dans la liste d'entier. 
      0 -> aucune sous-liste
      100 -> uniquement des sous-listes ordonnées 
  output : 
  - int list : liste d'entier de n éléments compris entre
  0 et max_val contenant des sous-listes ordonnées
  Note: la taille des sous-listes ordonnées est une valeur aléatoire
  entre 2 et 12. Idéalement, cette valeur devrait etre modifiable
  selon les paramètres de création de la liste principale.  
*)
let rec rnd_list_sub(n, max_val, percent : int * int * int ) : int list =
  if n <= 0
  then []
  else
    (
      let try_sublist : int = Random.int(100) in
      let rnd_sublist_len : int = Random.int(11) + 2 in
      if (try_sublist < percent && rnd_sublist_len <= n)
      then 
        (
          let updated_n : int = n - rnd_sublist_len in
          let (first_half, second_half) : (int * int) = ((updated_n/2), updated_n - (updated_n/2)) in 
          rnd_list_sub(first_half,max_val,percent)@_rnd_sublist(rnd_sublist_len, max_val, 0)@rnd_list_sub(second_half,max_val,percent)
        )
      else Random.int(max_val)::rnd_list_sub(n-1,max_val,percent)
    )
;;
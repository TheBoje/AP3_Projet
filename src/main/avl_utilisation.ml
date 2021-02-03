(* ================================================== *)
(* ================== MODULE AVL.ML ================= *)
(*
  Ce module a été créé par le groupe d'étudiants suivant :
  - Yann Berthelot
  - Louis Leenart
  - Alexis Louail
  Le contenu de ce module a été fait d'apres le sujet de
  projet d'Algorithmique et Programmation 3, et contient
  des exemples d'utilisation des fonction d'avl. D'autres
  exemples d'utilisation de ces fonctions sont inclues
  dans le fichier avl_plot.ml.
 *)
(* ================================================== *)


(* ================================================== *)
(* ================== IMPORTATIONS ================== *)
(* ================================================== *)

#directory "../usage";;

#load "btree.cmo";;
#load "bst.cmo";;
#use "ap2util.ml";;
#use "avl.ml";;

open Bst;;
open Btree;;

Random.self_init;;


(* ================================================== *)
(* ===================== TESTS ====================== *)
(* ================================================== *)


(*
Création et affichage d'un arbre avant et après 
une rotation droite
*)
let test_avl_rd : int t_avltree = 
  updateHeight(
      rooting((1, 2), 
              rooting((2, 1), 
                      rooting((3, 0), empty(), empty()),
                      rooting((4, 0), empty(), empty())
                ),
              rooting((5, 0), empty(), empty())
    ))
;;
show_avl_tree(test_avl_rd);; 
show_avl_tree(rd(test_avl_rd));;



(*
Création et affichage d'un arbre avant et après 
une rotation gauche
*)
let test_avl_rg : int t_avltree = 
  updateHeight(
      rooting((1, 2), 
              rooting((3, 0), empty(), empty()),
              rooting((2, 1), 
                      rooting((4, 0), empty(), empty()),
                      rooting((5, 0), empty(), empty())
                )
    ))
;;
show_avl_tree(test_avl_rg);; 
show_avl_tree(rg(test_avl_rg));;


(*
Création et affichage d'un arbre avant et après 
une rotation gauche droite
*)
let test_avl_rgd : int t_avltree = 
  updateHeight(rooting((6, 0), 
                       rooting((2, 2), 
                               rooting((1, 0), empty(), empty()),
                               rooting((4, 1), 
                                       rooting((3, 0), empty(), empty()),
                                       rooting((5, 0), empty(), empty())
                                 )
                         ),
                       rooting((7, 2), empty(), empty())
    ))
;;
show_avl_tree(test_avl_rgd);; 
show_avl_tree(rgd(test_avl_rgd));;


(*
Création et affichage d'un arbre avant et après 
une rotation droite gauche
*)
let test_avl_rdg : int t_avltree = 
  updateHeight(
      rooting((2, 3),
              rooting((1, 0), empty(), empty()),
              rooting((6, 2),
                      rooting((4, 1),
                              rooting((3, 0), empty(), empty()),
                              rooting((5, 0), empty(), empty())
                        ),
                      rooting((7, 0), empty(), empty())  
                )
    ))
;;
show_avl_tree(test_avl_rdg);; 
show_avl_tree(rdg(test_avl_rdg));;


(*
Nous réutilisons un des arbres précédents pour tester
le calcul du déséquilibre. Ici, nous utilisons aussi 
la fonction show_height_tree() pour afficher la hauteur
de chaque noeud de l'avl.
*)
let test_desequilibre : int t_avltree = test_avl_rd;;
show_avl_tree(test_desequilibre);;
show_height_tree(test_desequilibre);;
desequilibre(test_desequilibre);;


(*
Nous réutilisons un des arbres précédents pour tester
le rééquilibrage.
*)
let test_reequilibre: int t_avltree= test_avl_rdg;;
show_avl_tree(test_reequilibre);;
show_height_tree(test_reequilibre);;
show_avl_tree(reequilibrer(test_reequilibre));;
show_height_tree(reequilibrer(test_reequilibre));;


(*
Nous réutilisons un des arbres précédents pour tester
l'insertion d'une valeur dans un avl. On note que le 
déséquilibre de chaque noeud est compris entre 2 et -2.
Nous testons ensuite l'insertion d'une valeur dans un
avl qui est équilibré.
*)

let test_insert_avl: int t_avltree = test_avl_rdg;;
show_avl_tree(test_insert_avl);;
desequilibre(test_insert_avl);;
show_deseq_btree(test_insert_avl);;
show_avl_tree(reequilibrer(test_insert_avl));;
desequilibre(reequilibrer(test_insert_avl));;
show_deseq_btree(reequilibrer(test_insert_avl));;
show_avl_tree(insert_avl(8,reequilibrer(test_insert_avl)));;
desequilibre(insert_avl(8,reequilibrer(test_insert_avl)));;


(*
Nous suivons la meme logique avec la suppression d'une 
valeur dans l'avl que pour l'insertion.
*)
let test_supr_avl : int t_avltree = insert_avl(8,reequilibrer(test_insert_avl));;
show_avl_tree(test_supr_avl);;
desequilibre(test_supr_avl);;
show_avl_tree(suppr_avl(4,test_supr_avl));;
desequilibre(suppr_avl(4,test_supr_avl));;


(*
Nous testons maintenant la recherche dans l'avl.
On note que la fonction renvoie TRUE si la valeur
est dans l'avl, et FALSE si elle ne l'est pas.
*)
seek_avl(5,test_supr_avl);;
seek_avl(10,test_supr_avl);;



(*
Enfin nous testons ici la creation d'arbre équlibré
à partir de listes via la fonction avl_rnd_create().
On utilise aussi des listes générés via rnd_list_int()
et rnd_list_sub(). 
*)

let test_list: int list = [3;8;1;9;4;7];;
let test_create_avl: int t_avltree = avl_rnd_create(test_list);;
show_avl_tree(test_create_avl);;

show_avl_tree(avl_rnd_create(rnd_list_int(5, 100)));;
show_avl_tree(avl_rnd_create(rnd_list_int(6, 100)));;

show_avl_tree(avl_rnd_create(rnd_list_sub(100,100,50)));;
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
#directory "./";;

#use "AP2util.ml";;
#load "btree.cmo";;
#load "bst.cmo";;
open Bst;;
open Btree;;
#use "avl.ml";;
Random.self_init;;


(* ================================================== *)
(* ===================== TESTS ====================== *)
(* ================================================== *)


(*
ici nous testont la rotation droite pour cela on utilise test_avl_rd 
qui crée un arbre binaire equilibrer.
ensuite nous affichons l'arbre avant et apres la rottation
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
ici nous testont la rotation gauche pour cela on utilise test_avl_rg 
qui crée un arbre binaire equilibrer.
ensuite nous affichons l'arbre avant et apres la rottation
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
ici nous testont la rotation gauche droite pour cela on utilise test_avl_rgd 
qui crée un arbre binaire equilibrer.
ensuite nous affichons l'arbre avant et apres la rottation
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
ici nous testont la rotation droite gauche pour cela on utilise test_avl_rdg 
qui crée un arbre binaire equilibrer.
ensuite nous affichons l'arbre avant et apres la rottation
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
Nous allons reutiliser l'arbre du test roation droite pour tester la fonction de calculer du désiquilibre
d'un arbre.
Pour verifier cela on print l'arbe , puis on test show_height_tree qui montre la hauteur d'un noeud de l'arbre
enfin on test la fonction desequilibre
*)


let test_desequilibre : int t_avltree = test_avl_rd;;
show_avl_tree(test_desequilibre);;
show_height_tree(test_desequilibre);;
desequilibre(test_desequilibre);;


(*
Nous reutilison l'arbre de la rotation gauche droite pour tester les prochaines fonctions.
Nous commencons donc par affiché l'arbre puis ensuite la hauteur de l'arbre
une premiere fois puis ensuite on afficher l'arbre une seconde fosi mais avec
la fonction reequilibrer et on affiche un seconde fois la hauteur de l'arbre reequilibrer.
*)


let test_reequilibre: int t_avltree= test_avl_rdg;;
show_avl_tree(test_reequilibre);;
show_height_tree(test_reequilibre);;
show_avl_tree(reequilibrer(test_reequilibre));;
show_height_tree(reequilibrer(test_reequilibre));;

(*
Nous allons tester l'insertion dans un arbre.
Pour cela nous allons d'abord  afficher l'arbre et son déséquilibre une premiere fois avant la fonction.
Puis nous allons reafficher l'arbre et son déséquibilibre une seconde fois pour verifer que la fonction ai bien ajouter
l'entier et ai garder un déséquilibre <= 2 ou -2
*)

let test_insert_avl: int t_avltree = test_avl_rdg;;
show_avl_tree(test_insert_avl);;
desequilibre(test_insert_avl);;
show_deseq_btree(test_insert_avl);;
show_avl_tree(reequilibrer(test_insert_avl));;
desequilibre(reequilibrer(test_insert_avl));;
show_deseq_btree(reequilibrer(test_insert_avl));;
(* erreur ->*)
show_avl_tree(insert_avl(8,reequilibrer(test_insert_avl)));;
desequilibre(insert_avl(8,reequilibrer(test_insert_avl)));;


(*
Nous reproduisons le meme deroulement que pour le teste inser_avl 
mais cette fois avec la fonction supr_ avl qui renvoie
un arbre équilibré sans  élément demandé.
Pour cela on utiliseras un arbre spécifique que l'on definie a partir de l'arbre précédent 
 *)

let test_supr_avl : int t_avltree = insert_avl(8,reequilibrer(test_insert_avl));;
show_avl_tree(test_supr_avl);;
desequilibre(test_supr_avl);;
show_avl_tree(suppr_avl(4,test_supr_avl));;
desequilibre(suppr_avl(4,test_supr_avl));;

(*
En nous servant de l'arbre définit lors du test supr_avl nous
teston la fonction seek_val qui recherche si une valeur est
présente dans ette arbre et renvoi un booléen.
 *)

seek_avl(5,test_supr_avl);;
seek_avl(10,test_supr_avl);;



(*
Enfin nous teston ici la creation d'arbre équlibré a partir de listes
 grace a la fonctions avl_rnd_create. pour cela on utiliseras une liste connue
puis par la suite on utiliseras la fonction rnd_list_sub qui crée une liste 
aléatoire avec des entiers et des sous listes ordonées crée par la fonction
rnd_sublist.
Enfin on affiche l'arbre de cette lsite aléatoire.
 *)

let test_list: int list = [3;8;1;9;4;7];;
let test_create_avl: int t_avltree = avl_rnd_create(test_list);;
show_avl_tree(test_create_avl);;

show_avl_tree(avl_rnd_create(rnd_list_int(5, 100)));;
show_avl_tree(avl_rnd_create(rnd_list_int(6, 100)));;

_rnd_sublist( 10,10, 90);;
rnd_list_sub(20,100,50);;
show_avl_tree(avl_rnd_create(rnd_list_sub(100,100,50)));;

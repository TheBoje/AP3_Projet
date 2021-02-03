(* ================================================== *)
(* =============== MODULE AVL_PLOT.ML =============== *)
(*
  Ce module a été créé par le groupe d'étudiants suivant :
  - Yann Berthelot
  - Louis Leenart
  - Alexis Louail
  Le contenu de ce module a été fait d'apres le sujet de
  projet d'Algorithmique et Programmation 3, et est 
  responsable de l'affichage de la complexité des algor-
  ithmes des AVL (insert, suppr et seek).
*)
(* ================================================== *)

(* ================================================== *)
(* ================== IMPORTATIONS ================== *)
(* ================================================== *)

#directory "src/usage";;

#use "ap2util.ml";;
#load "graphics.cma";;
#use "graphics.ml";;
#load "btree.cmo";;
#load "bst.cmo";;
open Bst;;
open Btree;;
#use "avl.ml";;


(* ================================================== *)
(* ================ UTILITAIRES PLOT ================ *)
(* ================================================== *)

(* Valeur maximales contenues dans chaque noeud. Il est
  recommandé d'avoir valeur_max supérieur a la taille
  maximale des arbres que l'on souhaite tester *)
let valeur_max : int = 3000;;

(*
  [FONCTION PRIVÉE]
  Génère n avl de taille 1 à n et effectue une opération
  d'insertion. On conserve alors le temps d'execution 
  de chaque opération
  input : 
  - n : taille de l'avl le plus grand
  output :
  - float array (1) : liste des temps d'execution de chaque
  opération
  - float array (2) : indice de chaque opération
*)
let _insert_avl_compute(n : int) : float array * float array =
  let indices : float array = arr_make(n + 1, 0.0) in
  let temps : float array = arr_make(n + 1, 0.0) in
  (
    for i = 1 to n 
    do 
      if (i mod 10 = 0)
      then ignore(Printf.printf "%d\n" i; flush stdout);
      let avl = avl_rnd_create(rnd_list_int(i, valeur_max)) in
      temps.(i) <- Sys.time();
      ignore(insert_avl(Random.int(valeur_max), avl));
      temps.(i) <- Sys.time() -. temps.(i);
      indices.(i) <- float_of_int(i);
    done;
    (temps, indices);
  )
;;

(*
  Affiche le temps d'execution de l'opération d'insertion
  sur les avl, pour une taille d'avl allant de 1 à n.
  input : 
  - n : taille de l'avl le plus grand
  output :
  - float : temps total d'execution de la fonction.
  - unit : affichage du graphique de temps d'execution
  de l'opération d'insertion.
*)
let insert_avl_plot(n : int) : float =
  let init_time : float = Sys.time() in
  let (temps, indices) : float array * float array = _insert_avl_compute(n) in
  let repere : t_rep = {orx = 50; ory = 50; extx = 900; exty = 500} in
  (
    open_graph(1000, 600);
    clear_graph();
    draw_rep(repere);
    draw_curve(temps, indices, arr_len(indices) - 1, repere);
    Sys.time() -. init_time;
  )
;; 


(*
  [FONCTION PRIVÉE]
  Génère n avl de taille 1 à n et effectue une opération
  de suppression. On conserve alors le temps d'execution 
  de chaque opération
  input : 
  - n : taille de l'avl le plus grand
  output :
  - float array (1) : liste des temps d'execution de chaque
  opération
  - float array (2) : indice de chaque opération
*)
let _suppr_avl_compute(n : int) : float array * float array =
  let indices : float array = arr_make(n + 1, 0.0) in
  let temps : float array = arr_make(n + 1, 0.0) in
  (
    for i = 1 to n 
    do 
      if (i mod 10 = 0)
      then ignore(Printf.printf "%d\n" i; flush stdout);
      let rnd_list = rnd_list_int(i, valeur_max) in
      let avl = avl_rnd_create(rnd_list) in
      temps.(i) <- Sys.time();
      ignore(suppr_avl(nth(rnd_list, Random.int(len(rnd_list))), avl));
      temps.(i) <- Sys.time() -. temps.(i);
      indices.(i) <- float_of_int(i);
    done;
    (temps, indices);
  )
;;


(*
  Affiche le temps d'execution de l'opération de suppression
  sur les avl, pour une taille d'avl allant de 1 à n.
  input : 
  - n : taille de l'avl le plus grand
  output :
  - float : temps total d'execution de la fonction.
  - unit : affichage du graphique de temps d'execution
  de l'opération d'insertion.
*)
let suppr_avl_plot(n : int) : float =
  let init_time : float = Sys.time() in
  let (temps, indices) : float array * float array = _suppr_avl_compute(n) in
  let repere : t_rep = {orx = 50; ory = 50; extx = 900; exty = 500} in
  (
    open_graph(1000, 600);
    clear_graph();
    draw_rep(repere);
    draw_curve(temps, indices, arr_len(indices) - 1, repere);
    Sys.time() -. init_time;
  )
;;


(*
  [FONCTION PRIVÉE]
  Génère n avl de taille 1 à n et effectue une opération
  de recherche. On conserve alors le temps d'execution 
  de chaque opération
  input : 
  - n : taille de l'avl le plus grand
  output :
  - float array (1) : liste des temps d'execution de chaque
  opération
  - float array (2) : indice de chaque opération
*)
let _seek_avl_compute(n : int) : float array * float array =
  let indices : float array = arr_make(n + 1, 0.0) in
  let temps : float array = arr_make(n + 1, 0.0) in
  (
    for i = 1 to n 
    do 
      if (i mod 10 = 0)
      then ignore(Printf.printf "%d\n" i; flush stdout);
      let rnd_list = rnd_list_int(i, valeur_max) in
      let avl = avl_rnd_create(rnd_list) in
      temps.(i) <- Sys.time();
      ignore(seek_avl(nth(rnd_list, Random.int(len(rnd_list))), avl));
      temps.(i) <- Sys.time() -. temps.(i);
      indices.(i) <- float_of_int(i);
    done;
    (temps, indices);
  )
;;


(*
  Affiche le temps d'execution de l'opération de recherche
  sur les avl, pour une taille d'avl allant de 1 à n.
  input : 
  - n : taille de l'avl le plus grand
  output :
  - float : temps total d'execution de la fonction.
  - unit : affichage du graphique de temps d'execution
  de l'opération d'insertion.
*)
let seek_avl_plot(n : int) : float =
  let init_time : float = Sys.time() in
  let (temps, indices) : float array * float array = _suppr_avl_compute(n) in
  let repere : t_rep = {orx = 50; ory = 50; extx = 900; exty = 500} in
  (
    open_graph(1000, 600);
    clear_graph();
    draw_rep(repere);
    draw_curve(temps, indices, arr_len(indices) - 1, repere);
    Sys.time() -. init_time;
  )
;;


(* ================================================== *)
(* ==================== PLOT EXEC =================== *)
(* ================================================== *)

(*
insert_avl_plot(1000);;
*)
(*
suppr_avl_plot(1000);; 
 *)
(*
seek_avl_plot(1000);;
 *)

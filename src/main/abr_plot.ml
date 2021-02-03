(* ================================================== *)
(* =============== MODULE ABR_PLOT.ML =============== *)
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

#directory "../usage";;

#load "btree.cmo";;
#load "bst.cmo";;
#load "graphics.cma";;
#use "ap2util.ml";;
#use "graphics.ml";;
#use "abr.ml";;

open Btree;;
open List;;


(* ================================================== *)
(* ================ UTILITAIRES PLOT ================ *)
(* ================================================== *)


(*
  [DESCRIPTION]
  input : 
  - 
  output :
  - 
 *)
let bst_rnd_compute(n : int) : float array * float array =
  let indices : float array = arr_make(n + 1, 0.0) in
  let desec : float array = arr_make(n + 1, 0.0) in
  let sum : float ref = ref 0. in
  (
    for i = 1 to n 
    do 
      if (i mod 10 = 0)
      (*
       affichage toutes les 10 opération 
       pour connaitre l'avancement du programme
      *)
      then ignore(Printf.printf "%d/%d\n" i n; flush stdout);
      let bst = bst_rnd_create(i) in
      let desec_val : float = float_of_int(unbalance(bst)) in
      sum := !sum +. desec_val;
      desec.(i) <- (!sum /. float_of_int(i));
      indices.(i) <- float_of_int(i);
    done;
    (desec, indices);
  )
;;


(*
  [DESCRIPTION]
  input : 
  - 
  output :
  - 
 *)
let bst_rnd_plot(n : int) : float =
  let init_time : float = Sys.time() in
  let (desec, indices) : float array * float array = bst_rnd_compute(n) in
  let repere : t_rep = {orx = 50; ory = 50; extx = 900; exty = 500} in
  (
    open_graph(1000, 600);
    clear_graph();
    draw_rep(repere);
    draw_curve(desec, indices, arr_len(indices) - 1, repere);
    Sys.time() -. init_time;
  )
;;


(*
  [DESCRIPTION]
  input : 
  - 
  output :
  - 
 *)
let bst_rnd_sublist_compute(n : int) : float array * float array =
  let indices : float array = arr_make(n + 1, 0.0) in
  let desec : float array = arr_make(n + 1, 0.0) in
  let sum : float ref = ref 0. in
  (
    for i = 1 to n 
    do 
      if (i mod 10 = 0)
      (*
       affichage toutes les 10 opération 
       pour connaitre l'avancement du programme
      *)
      then ignore(Printf.printf "%d/%d\n" i n; flush stdout);
      let bst = bst_mix_create(i) in
      let desec_val : float = float_of_int(unbalance(bst)) in
      sum := !sum +. desec_val;
      desec.(i) <- (!sum /. float_of_int(i));
      indices.(i) <- float_of_int(i);
    done;
    (desec, indices);
  )
;;


(*
  [DESCRIPTION]
  input : 
  - 
  output :
  - 
 *)
let bst_rnd_sublist_plot(n : int) : float =
  let init_time : float = Sys.time() in
  let (desec, indices) : float array * float array = bst_rnd_sublist_compute(n) in
  let repere : t_rep = {orx = 50; ory = 50; extx = 900; exty = 500} in
  (
    open_graph(1000, 600);
    clear_graph();
    draw_rep(repere);
    draw_curve(desec, indices, arr_len(indices) - 1, repere);
    Sys.time() -. init_time;
  )
;;

(*
bst_rnd_plot(1000);;
bst_rnd_sublist_plot(1000);;
*)
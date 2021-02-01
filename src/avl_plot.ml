#directory "./";;

#use "AP2util.ml";;
#load "graphics.cma";;
#use "graphics.ml";;
#load "btree.cmo";;
#load "bst.cmo";;
open Bst;;
open Btree;;
#use "avl.ml";;


(* Tests de complexité *)

let valeur_max : int = 3000;;
let multiplicateur_max : float = 1000.;;
let step : int = 1;;
(* this thing doesnt work *)
let clear_values (data : float array) : float array =
  for i = 5 to (arr_len(data) - 2) / step
  do 
    let (act, min1, min2, plus1) : (float * float * float * float) = (data.(i * step), data.(i * step - 1), data.(i * step - 2), data.(i * step + 1)) in
    let past_diff : float = min1 -. min2 in
    if (act > ((past_diff +. min1) *. multiplicateur_max) || act < ((past_diff +. min1) *. multiplicateur_max )) (* //TODO Better condition? *)
    then data.(i * step) <- (min1 +. abs_float(past_diff)) (* thats not pretty i know*)
  done;
  (data)
;;

let insert_avl_compute(n : int) : float array * float array =
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

let insert_avl_plot(n : int) : float =
  let init_time : float = Sys.time() in
  let (temps, indices) : float array * float array = insert_avl_compute(n) in
  let repere : t_rep = {orx = 50; ory = 50; extx = 900; exty = 500} in
  (
    open_graph(1000, 600);
    clear_graph();
    draw_rep(repere);
    draw_curve(temps, indices, arr_len(indices) - 1, repere);
    Sys.time() -. init_time;
  )
;;

let suppr_avl_compute(n : int) : float array * float array =
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

let suppr_avl_plot(n : int) : float =
  let init_time : float = Sys.time() in
  let (temps, indices) : float array * float array = suppr_avl_compute(n) in
  let repere : t_rep = {orx = 50; ory = 50; extx = 900; exty = 500} in
  (
    open_graph(1000, 600);
    clear_graph();
    draw_rep(repere);
    draw_curve(temps, indices, arr_len(indices) - 1, repere);
    Sys.time() -. init_time;
  )
;;


let seek_avl_compute(n : int) : float array * float array =
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

let seek_avl_plot(n : int) : float =
  let init_time : float = Sys.time() in
  let (temps, indices) : float array * float array = suppr_avl_compute(n) in
  let repere : t_rep = {orx = 50; ory = 50; extx = 900; exty = 500} in
  (
    open_graph(1000, 600);
    clear_graph();
    draw_rep(repere);
    draw_curve(temps, indices, arr_len(indices) - 1, repere);
    Sys.time() -. init_time;
  )
;;

insert_avl_plot(1000);;

(*
suppr_avl_plot(300);; 
 *)
(*
seek_avl_plot(300);;
 *)

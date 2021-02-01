#load "btree.cmo";;
#load "bst.cmo";;
#use "AP2util.ml";;
#load "graphics.cma";;
#use "graphics.ml";;
#use "abr.ml";;

open Btree;;
open List;;


let bst_rnd_compute(n : int) : float array * float array =
  let indices : float array = arr_make(n + 1, 0.0) in
  let desec : float array = arr_make(n + 1, 0.0) in
  (
    for i = 1 to n 
    do 
      if (i mod 10 = 0)
      then ignore(Printf.printf "%d\n" i; flush stdout);
      let bst = bst_rnd_create(i) in
      desec.(i) <- float_of_int(unbalance(bst));
      indices.(i) <- float_of_int(i);
    done;
    (desec, indices);
  )
;;

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

bst_rnd_plot(10000);;


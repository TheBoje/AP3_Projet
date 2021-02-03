let open_graph(dx, dy : int * int) : unit = 
  let s = ":0 "^string_of_int(dx)^"x"^string_of_int(dy) in
     Graphics.open_graph s ;;

let close_graph() : unit = Graphics.close_graph() ;;

let clear_graph() : unit = Graphics.clear_graph() ;;

let resize_window(x, y) = Graphics.resize_window x y ;;




let moveto(x,y : int * int) : unit = Graphics.moveto x y ;;

let lineto(x, y : int * int) = Graphics.lineto x y ;;

let plot(x, y : int * int) = Graphics.plot x y ;;

let current_point() = Graphics.current_point() ;;

let draw_poly_line(t) = Graphics.draw_poly_line t ;;

let draw_circle(x, y, r : int * int * int) = Graphics.draw_circle x y r ;;

let draw_rect(x,y,dx,dy : int * int * int * int) = Graphics.draw_rect x y dx dy ;;

let fill_poly(t) = Graphics.draw_poly_line t ;;

let fill_circle(x, y, r : int * int * int) = Graphics.draw_circle x y r ;;

let fill_rect(x,y,dx,dy : int * int * int * int) = Graphics.draw_rect x y dx dy ;;

let set_line_width(e) = Graphics.set_line_width e ;;

let draw_str(s : string) : unit = Graphics.draw_string s ;;



type t_color = Graphics.color ;;


let set_color(color : t_color) = Graphics.set_color color ;;

let black : t_color = Graphics.black ;;
let blue : t_color = Graphics.blue ;;
let red : t_color = Graphics.red ;;
let green : t_color = Graphics.green ;;
let white : t_color = Graphics.white ;;
let yellow : t_color = Graphics.yellow ;;
let cyan : t_color = Graphics.cyan ;;
let magenta : t_color = Graphics.magenta ;;

type t_rep = {orx : int ; ory : int ; extx : int ; exty : int} ;;


let draw_rep(rep : t_rep) : unit =
  moveto(rep.orx, rep.ory) ; lineto(rep.orx + rep.extx + 50, rep. ory) ;
  moveto(rep.orx, rep.ory) ; lineto(rep.orx, rep.ory + rep.exty + 50) ;
;;

let comp_rep(rep, xmin, xmax, sx, ymin, ymax, sy) = 
  moveto(rep.orx  + 10, rep.ory - 20) ; draw_str(string_of_float(xmin)) ;
  moveto(rep.orx + rep.extx-10, rep.ory - 20) ; draw_str(string_of_float(xmax)) ;
  moveto(rep.orx + rep.extx/2, rep.ory - 20) ; draw_str(sx) ;
  moveto(rep.orx - 20, rep.ory + 10) ; draw_str(string_of_float(ymin)) ;
  moveto(rep.orx - 20, rep.ory + rep.exty-10) ; draw_str(string_of_float(ymax)) ;
  moveto(rep.orx - 20, rep.ory + rep.exty/2) ; draw_str(sy)
;;

let min_max_float(t, ind_max : float array * int) : float * float =
  if ind_max < 0 
  then failwith "erreur min_max_float : tableau vide"
  else
    let min : float ref = ref (t.(0)) and max : float ref = ref (t.(0)) in
      for i = 1 to ind_max 
      do
        if !min > t.(i) then min := t.(i) ;
        if !max < t.(i) then max := t.(i) ;
      done ;
(!min, !max)
;;


let draw_curve(arr_val, arr_ind, ind_max, rep : float array * float array * int * t_rep) : unit =
  let (minx, maxx) : float * float = min_max_float(arr_ind, ind_max) in
  let (miny, maxy) : float * float = min_max_float(arr_val, ind_max) in
  let dx : float = float_of_int(rep.extx) /.  (maxx -. minx) in
  let dy : float = float_of_int(rep.exty) /. (maxy -. miny) in
  let x : int ref = ref 0 and y : int ref = ref 0 in
    (
    comp_rep(rep, minx, maxx, "taille", miny, maxy, "sec") ;
    moveto(rep.orx, rep.ory) ; 
    for i = 1 to ind_max 
    do
      x := rep.orx + int_of_float((arr_ind.(i) -. minx) *. dx) ;
      y := rep.ory + int_of_float((arr_val.(i) -. miny)*. dy) ;
      lineto(!x, !y)
    done
  )
;;

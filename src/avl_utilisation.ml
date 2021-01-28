#directory "./";;

#use "AP2util.ml";;
#load "btree.cmo";;
#load "bst.cmo";;
open Bst;;
open Btree;;

(*TESTS*)

let test_avl_rd : int t_avltree = 
  rooting(1, 
          rooting(2, 
                  rooting(3, empty(), empty()),
                  rooting(4, empty(), empty())
            ),
          rooting(5, empty(), empty())
    )
;;
show_int_btree(test_avl_rd);; 
show_int_btree(rd(test_avl_rd));;


let test_avl_rg : int t_avltree = 
  rooting(1, 
          rooting(3, empty(), empty()),
          rooting(2, 
                  rooting(4, empty(), empty()),
                  rooting(5, empty(), empty())
            )
    )
;;
show_int_btree(test_avl_rg);; 
show_int_btree(rg(test_avl_rg));;


let test_avl_rgd : int t_avltree = 
  rooting(6, 
          rooting(2, 
                  rooting(1, empty(), empty()),
                  rooting(4, 
                          rooting(3, empty(), empty()),
                          rooting(5, empty(), empty())
                    )
            ),
          rooting(7, empty(), empty())
    )
;;
show_int_btree(test_avl_rgd);; 
show_int_btree(rgd(test_avl_rgd));;


let test_avl_rdg : int t_avltree =
  rooting(2,
          rooting(1, empty(), empty()),
          rooting(6,
                  rooting(4,
                          rooting(3, empty(), empty()),
                          rooting(5, empty(), empty())
                    ),
                  rooting(7, empty(), empty())  
            )
    )
;;
show_int_btree(test_avl_rdg);; 
show_int_btree(rdg(test_avl_rdg));;


let test_desequilibre : int t_avltree = test_avl_rd;;
show_int_btree(test_desequilibre);;
desequilibre(test_desequilibre);;


let test_reequilibre: int t_avltree= test_avl_rdg;;
show_int_btree(test_reequilibre);;
show_int_btree(reequilibrer(test_reequilibre));;


let test_insert_avl: int t_avltree= test_avl_rdg;;
show_int_btree(test_insert_avl);;
desequilibre(test_insert_avl);;
show_int_btree(insert_avl(8,test_insert_avl));;
desequilibre((insert_avl(8,test_insert_avl)));;


let test_supr_avl : int t_avltree= insert_avl(8,test_insert_avl);;
show_int_btree(test_supr_avl);;
desequilibre(test_supr_avl);;
show_int_btree(suppr_avl(4,test_supr_avl));;
desequilibre(suppr_avl(4,test_supr_avl));;

bst_seek(5,test_supr_avl);;
bst_seek(10,test_supr_avl);;
(* TODO Note : la fonction bst_seek fonctionne toujours pour les avl *)


let test_list: int list = [3;8;1;9;4;7];;
let test_create_avl: int t_avltree = avl_rnd_create(test_list);;
show_int_btree(test_create_avl);;

show_int_btree(avl_rnd_create(random_list_int(5, 100)));;
show_int_btree(avl_rnd_create(random_list_int(6, 100)));;

random_sublist( 5,100 ,0 );;

show_int_btree(avl_rnd_create(random_list_sub_int(10,100,10)));;
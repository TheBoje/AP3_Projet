#directory "./";;

#use "AP2util.ml";;
#load "btree.cmo";;
#load "bst.cmo";;
open Bst;;
open Btree;;
#use "avl.ml";;
Random.self_init;;


(*TESTS*)

let test_avl_rd : int t_avltree = updateHeight(
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


let test_avl_rg : int t_avltree = updateHeight(
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


let test_avl_rgd : int t_avltree = updateHeight(rooting((6, 0), 
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


let test_avl_rdg : int t_avltree = updateHeight(
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


let test_desequilibre : int t_avltree = test_avl_rd;;
show_avl_tree(test_desequilibre);;
show_height_tree(test_desequilibre);;
desequilibre(test_desequilibre);;


let test_reequilibre: int t_avltree= test_avl_rdg;;
show_avl_tree(test_reequilibre);;
show_height_tree(test_reequilibre);;
show_avl_tree(reequilibrer(test_reequilibre));;
show_height_tree(reequilibrer(test_reequilibre));;



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


let test_supr_avl : int t_avltree = insert_avl(8,reequilibrer(test_insert_avl));;
show_avl_tree(test_supr_avl);;
desequilibre(test_supr_avl);;
show_avl_tree(suppr_avl(4,test_supr_avl));;
desequilibre(suppr_avl(4,test_supr_avl));;

seek_avl(5,test_supr_avl);;
seek_avl(10,test_supr_avl);;


let test_list: int list = [3;8;1;9;4;7];;
let test_create_avl: int t_avltree = avl_rnd_create(test_list);;
show_avl_tree(test_create_avl);;

show_avl_tree(avl_rnd_create(rnd_list_int(5, 100)));;
show_avl_tree(avl_rnd_create(rnd_list_int(6, 100)));;

rnd_sublist( 10,10, 90);;
rnd_list_sub(20,100,50);;
show_avl_tree(avl_rnd_create(rnd_list_sub(100,100,50)));;

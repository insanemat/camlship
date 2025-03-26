(**Test des fonctions de battleship.ml
    @author Sarah Favre
    @author Marius Roumy
    @author Anne-Celia Mensah
    @author Maêl Icapi

    @version 1.0 
*)

#use "CPtest.ml";;
#use "battleship.ml";;

(** Vérifie si les paramètres de création de la fenêtres sont conformes à la spécification, le test levera une erreur le cas échéant
    @author Anne Celia Mensah 
    @author Maël ICAPI
    @author Marius Roumy
    @return unit
*)
let test_fonc_init_params() : unit =
    let l_res : t_params t_test_result = test_exec (init_params, "Valeurs des paramètres conformes à la spécification", ()) in
    assert_true(test_is_success(l_res));
    assert_equals_result ({margin = 30 ; cell_size = 15 ; grid_size = 24 ; window_width = 670 ; window_height = 390; message_size = 60 } , l_res)
;;

let test_fonc_display_empty_grids() : unit =
    let l_res : unit t_test_result = test_exec(display_empty_grids, "Creé bien l'affichage", init_params()) in
    assert_true(test_is_success(l_res))
;;

(**fonction de test qui test un certain placement automatique des bateaux
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_auto_placing_ship_1(): unit =
;;

(**fonction de test qui test si le calcule de la liste des positions d’un bateau à placer est correct
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_positions_list1(): unit = 
    let l_res : (int * int) list t_test_result = test_exec (positions_list, "donne la bonne position quand le bateau est orienté vers le haut",
                                                             {ship_type = PORTE_AVION; x = 7; y =  0; direction = 2; size = 5}) in
    assert_true(test_is_success(l_res));
    assert_equals_result([(7, 0); (7, -1); (7, -2); (7, -3); (7, -4)], l_res)
;;

(**fonction de test qui test si la règle de placement voulu est bien respectée
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_can_place_ship(): unit =
;;

(**fonction de test qui test si les cases coloriées sont les bonnes, dans la bonne grille et de la bonne couleur
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)

let test_fonc_display_grid(): unit =
;;

(**fonction de test qui test si la case coleriée est de la bonne couleur
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_color_cell(): unit =
;;

(**fonction de test qui test si la position du pixel demandé est la bonne
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_cell_to_pixel(): unit =
;;

let do_test(): unit =
    test_fonc_positions_list1();
    test_report();
    test_reset_report();
;;


do_test() ;;
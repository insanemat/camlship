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
let test_fonc_positions_list_bas(): unit = 
    let l_res : (int * int) list t_test_result = test_exec (positions_list, "donne la bonne position quand le bateau est orienté vers le bas",
                                                             {ship_type = PORTE_AVION; x = 7; y =  0; direction = 2; size = 5}) in
    assert_true(test_is_success(l_res));
    assert_equals_result([(7, 0); (7, -1); (7, -2); (7, -3); (7, -4)], l_res)
;;

(**fonction de test qui test si le calcule de la liste des positions d’un bateau à placer est correct
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_positions_list_droite(): unit = 
    let l_res : (int * int) list t_test_result = test_exec (positions_list, "donne la bonne position quand le bateau est orienté vers la droite",
                                                             {ship_type = PORTE_AVION; x = 7; y =  0; direction = 1; size = 5}) in
    assert_true(test_is_success(l_res));
    assert_equals_result([(7, 0); (8, 0); (9, 0); (10, 0); (11, 0)], l_res)
;;

(**fonction de test qui test si le calcule de la liste des positions d’un bateau à placer est correct
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_positions_list_haut(): unit = 
    let l_res : (int * int) list t_test_result = test_exec (positions_list, "donne la bonne position quand le bateau est orienté vers le haut",
                                                             {ship_type = PORTE_AVION; x = 7; y =  0; direction = 0; size = 5}) in
    assert_true(test_is_success(l_res));
    assert_equals_result([(7, 0); (7, 1); (7, 2); (7, 3); (7, 4)], l_res)
;;

(**fonction de test qui test si le calcule de la liste des positions d’un bateau à placer est correct
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_positions_list_gauche(): unit = 
    let l_res : (int * int) list t_test_result = test_exec (positions_list, "donne la bonne position quand le bateau est orienté vers la gauche",
                                                             {ship_type = PORTE_AVION; x = 7; y =  0; direction = 3; size = 5}) in
    assert_true(test_is_success(l_res));
    assert_equals_result([(7, 0); (6, 0); (5, 0); (4, 0); (3, 0)], l_res)
;;

(**fonction de test qui test si le calcule de la liste des positions d’un bateau à placer est correct
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_positions_list_2(): unit = 
    let l_res : (int * int) list t_test_result = test_exec (positions_list, "donne le bon nombre d'élément dans la liste",
                                                             {ship_type = PORTE_AVION; x = 7; y =  0; direction = 4; size = 2}) in
    assert_true(test_is_success(l_res));
    assert_equals_result([(7, 0); (6, 0)], l_res)
;;


(**fonction de test qui test si le calcule de la liste des positions d’un bateau à placer est correct
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_positions_list_3(): unit = 
    let l_res : (int * int) list t_test_result = test_exec (positions_list, "donne le bon nombre d'élément dans la liste",
                                                             {ship_type = CONTRE_TORPILLEUR; x = 7; y =  0; direction = 5; size = 3}) in
    assert_true(test_is_success(l_res));
    assert_equals_result([(7, 0); (6, 0); (5, 0)], l_res)
;;


(**fonction de test qui test si le calcule de la liste des positions d’un bateau à placer est correct
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_positions_list_4(): unit = 
    let l_res : (int * int) list t_test_result = test_exec (positions_list, "donne le bon nombre d'élément dans la liste",
                                                             {ship_type = CROISEUR; x = 7; y =  0; direction = 6; size = 4}) in
    assert_true(test_is_success(l_res));
    assert_equals_result([(7, 0); (6, 0); (5, 0); (4, 0)], l_res)
;;

(**fonction de test qui test si le calcule de la liste des positions d’un bateau à placer est correct
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_positions_list_5(): unit = 
    let l_res : (int * int) list t_test_result = test_exec (positions_list, "donne le bon nombre d'élément dans la liste",
                                                             {ship_type = PORTE_AVION; x = 7; y =  0; direction = 7; size = 5}) in
    assert_true(test_is_success(l_res));
    assert_equals_result([(7, 0); (6, 0); (5, 0); (4, 0); (3, 0)], l_res)
;;

(**fonction de test qui test si la règle de placement voulu est bien respectée
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_can_place_ship(): unit =
    let l_res : bool list t_test_result = test_exec (can_place_ship, "donne true si la position respecte les critères",
     ([|[|None ; None ; None ; None ; None; None; None ; None ; None ; None |]
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 1 ; direction = 0 ; size = 1} ; None; None; None ; None ; None|],
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 2 ; direction = 0 ; size = 2} ; None; None; None ; None ; None|],
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 3 ; direction = 0 ; size = 3} ; None; None; None ; None ; None|],
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 4 ; direction = 0 ; size = 4} ; None; None; None ; None ; None|],
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 5 ; direction = 0 ; size = 5} ; None; None; None ; None ; None|],
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|],
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|],
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|],
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|]|], Some {ship_type = CROISEUR; x = 8; y = 5; direction = 0; size = 4})) in
    assert_true(test_is_success(l_res));
    assert_equals_result(true, l_res);
;;

(**fonction de test qui test si la règle de placement voulu est bien respectée
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_can_place_ship(): unit =
    let l_res : bool list t_test_result = test_exec (can_place_ship, "donne false si la position ne respecte pas les critères", 
                                                            {ship_type = CROISEUR; x = 8; y = 5; direction = 0; size = 4}) in
    assert_true(test_is_success(l_res));
    assert_equals_result(false, l_res);
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
    test_fonc_positions_list_haut();
    test_fonc_positions_list_droite();
    test_fonc_positions_list_bas();
    test_fonc_positions_list_gauche();
    test_fonc_positions_list_2();
    test_fonc_positions_list_3();
    test_fonc_positions_list_4();
    test_fonc_positions_list_5();
    test_report();
    test_reset_report();
;;


do_test() ;;

[|  [| ; ; ; ; ; ; ; ; ; |],
    [| ; ; ; ; Some {ship_type = PORTE_AVION; x = 4; y = 1 ; direction = 0 ; size = 1} ; ; ; ; ; |],
    [| ; ; ; ; {ship_type = PORTE_AVION; x = 4; y = 2 ; direction = 0 ; size = 2} ; ; ; ; ; |],
    [| ; ; ; ; {ship_type = PORTE_AVION; x = 4; y = 3 ; direction = 0 ; size = 3} ; ; ; ; ; |],
    [| ; ; ; ; {ship_type = PORTE_AVION; x = 4; y = 4 ; direction = 0 ; size = 4} ; ; ; ; ; |],
    [| ; ; ; ; {ship_type = PORTE_AVION; x = 4; y = 5 ; direction = 0 ; size = 5} ; ; ; ; ; |],
    [| ; ; ; ; ; ; ; ; ; |],
    [| ; ; ; ; ; ; ; ; ; |],
    [| ; ; ; ; ; ; ; ; ; |],
    [| ; ; ; ; ; ; ; ; ; |],|]

    {ship_type = PORTE_AVION; x = 4; y = 5 ; direction = 0 ; size = 5}

    Array.make_matrix 10 10 None;;
    
    auto_placing_ships((Array.make_matrix 10 10 None), [PORTE_AVION ; CROISEUR; CONTRE_TORPILLEUR; CONTRE_TORPILLEUR; TORPILLEUR]) ;;
    
    Array.m
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
let test_fonc_can_place_ship_haut_true(): unit =
    let l_res : bool t_test_result = test_exec (can_place_ship, "donne true si la direction orientée vers le haut respecte les critères",
     ([|[|None ; None ; None ; None ; None; None; None ; None ; None ; None |];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 1 ; direction = 0 ; size = 1} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 2 ; direction = 0 ; size = 2} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 3 ; direction = 0 ; size = 3} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 4 ; direction = 0 ; size = 4} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 5 ; direction = 0 ; size = 5} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|]|], {ship_type = CROISEUR; x = 8; y = 5; direction = 0; size = 4})) in
    assert_true(test_is_success(l_res));
    assert_equals_result(true, l_res);
;;

(**fonction de test qui test si la règle de placement voulu est bien respectée
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_can_place_ship_bas_true(): unit =
    let l_res : bool t_test_result = test_exec (can_place_ship, "donne true si la direction orientée vers le bas respecte les critères",
     ([|[|None ; None ; None ; None ; None; None; None ; None ; None ; None |];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 1 ; direction = 0 ; size = 1} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 2 ; direction = 0 ; size = 2} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 3 ; direction = 0 ; size = 3} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 4 ; direction = 0 ; size = 4} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 5 ; direction = 0 ; size = 5} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|]|], {ship_type = CROISEUR; x = 8; y = 5; direction = 2; size = 4})) in
    assert_true(test_is_success(l_res));
    assert_equals_result(true, l_res);
;;

(**fonction de test qui test si la règle de placement voulu est bien respectée
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_can_place_ship_gauche_true(): unit =
    let l_res : bool t_test_result = test_exec (can_place_ship, "donne true si la direction orientée vers la gauche respecte les critères",
     ([|[|None ; None ; None ; None ; None; None; None ; None ; None ; None |];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 1 ; direction = 0 ; size = 1} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 2 ; direction = 0 ; size = 2} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 3 ; direction = 0 ; size = 3} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 4 ; direction = 0 ; size = 4} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 5 ; direction = 0 ; size = 5} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|]|], {ship_type = CROISEUR; x = 8; y = 5; direction = 3; size = 4})) in
    assert_true(test_is_success(l_res));
    assert_equals_result(true, l_res);
;;

(**fonction de test qui test si la règle de placement voulu est bien respectée
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_can_place_ship_droite_true(): unit =
    let l_res : bool t_test_result = test_exec (can_place_ship, "donne true si la direction orientée vers la droite respecte les critères",
     ([|[|None ; None ; None ; None ; None; None; None ; None ; None ; None |];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 1 ; direction = 0 ; size = 1} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 2 ; direction = 0 ; size = 2} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 3 ; direction = 0 ; size = 3} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 4 ; direction = 0 ; size = 4} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 5 ; direction = 0 ; size = 5} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|]|], {ship_type = CROISEUR; x = 5; y = 5; direction = 1; size = 4})) in
    assert_true(test_is_success(l_res));
    assert_equals_result(true, l_res);
;;

(**fonction de test qui test si la règle de placement voulu est bien respectée
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_can_place_ship_superposition(): unit =
    let l_res : bool t_test_result = test_exec (can_place_ship, "donne false si le bateau se superpose avec un autre",
     ([|[|None ; None ; None ; None ; None; None; None ; None ; None ; None |];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 1 ; direction = 0 ; size = 1} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 2 ; direction = 0 ; size = 2} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 3 ; direction = 0 ; size = 3} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 4 ; direction = 0 ; size = 4} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 5 ; direction = 0 ; size = 5} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|]|], {ship_type = CROISEUR; x = 3; y = 5; direction = 1; size = 4})) in
    assert_true(test_is_success(l_res));
    assert_equals_result(false, l_res);
;;

(**fonction de test qui test si la règle de placement voulu est bien respectée
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_can_place_ship_sortie_haut(): unit =
    let l_res : bool t_test_result = test_exec (can_place_ship, "donne false si le bateau depasse la grille par le haut",
     ([|[|None ; None ; None ; None ; None; None; None ; None ; None ; None |];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 1 ; direction = 0 ; size = 1} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 2 ; direction = 0 ; size = 2} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 3 ; direction = 0 ; size = 3} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 4 ; direction = 0 ; size = 4} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 5 ; direction = 0 ; size = 5} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|]|], {ship_type = CROISEUR; x = 0; y = 0; direction = 0; size = 4})) in
    assert_true(test_is_success(l_res));
    assert_equals_result(false, l_res);
;;

(**fonction de test qui test si la règle de placement voulu est bien respectée
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_can_place_ship_sortie_droite(): unit =
    let l_res : bool t_test_result = test_exec (can_place_ship, "donne false si le bateau depasse la grille par la droite",
     ([|[|None ; None ; None ; None ; None; None; None ; None ; None ; None |];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 1 ; direction = 0 ; size = 1} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 2 ; direction = 0 ; size = 2} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 3 ; direction = 0 ; size = 3} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 4 ; direction = 0 ; size = 4} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 5 ; direction = 0 ; size = 5} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|]|], {ship_type = CROISEUR; x = 8; y = 7; direction = 1; size = 4})) in
    assert_true(test_is_success(l_res));
    assert_equals_result(false, l_res);
;;

(**fonction de test qui test si la règle de placement voulu est bien respectée
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_can_place_ship_sortie_bas(): unit =
    let l_res : bool t_test_result = test_exec (can_place_ship, "donne false si le bateau depasse la grille par le bas",
     ([|[|None ; None ; None ; None ; None; None; None ; None ; None ; None |];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 1 ; direction = 0 ; size = 1} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 2 ; direction = 0 ; size = 2} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 3 ; direction = 0 ; size = 3} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 4 ; direction = 0 ; size = 4} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 5 ; direction = 0 ; size = 5} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|]|], {ship_type = CROISEUR; x = 9; y = 0; direction = 2; size = 4})) in
    assert_true(test_is_success(l_res));
    assert_equals_result(false, l_res);
;;

(**fonction de test qui test si la règle de placement voulu est bien respectée
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_can_place_ship_sortie_gauche(): unit =
    let l_res : bool t_test_result = test_exec (can_place_ship, "donne false si le bateau depasse la grille par la gauche",
     ([|[|None ; None ; None ; None ; None; None; None ; None ; None ; None |];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 1 ; direction = 0 ; size = 1} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 2 ; direction = 0 ; size = 2} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 3 ; direction = 0 ; size = 3} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 4 ; direction = 0 ; size = 4} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 5 ; direction = 0 ; size = 5} ; None; None; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
        [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|]|], {ship_type = CROISEUR; x = 1; y = 7; direction = 3; size = 4})) in
    assert_true(test_is_success(l_res));
    assert_equals_result(false, l_res);
;;

(**fonction de test qui test si la position du pixel demandé est la bonne
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_cell_to_pixel(): unit =
    let l_res = (int * int) t_test_result = test_exec(cell_to_pixel, "donne la bonne position du pixel en bas à gauche de la case donnée (0,0)", 
        ((x = 0, y = 0),((0, 0),
        [|[|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|]|]))) in
    assert_true(test_is_success(l_res))
    assert_equals_result(54, 306)
;;

(**fonction de test qui test si la position du pixel demandé est la bonne
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_cell_to_pixel2(): unit =
    let l_res = (int * int) t_test_result = test_exec(cell_to_pixel, "donne la bonne position du pixel en bas à gauche de la case donnée (0,1)", 
        ((x = 0, y = 0),((0, 0),
        [|[|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|]|]))) in
    assert_true(test_is_success(l_res))
    assert_equals_result(54, 282)
;;

(**fonction de test qui test si la position du pixel demandé est la bonne
    @author Sarah Favre
    @author Marius Roumy
    @return unit
*)
let test_fonc_cell_to_pixel3(): unit =
    let l_res = (int * int) t_test_result = test_exec(cell_to_pixel, "donne la bonne position du pixel en bas à gauche de la case donnée (1,0)", 
        ((x = 0, y = 0),((0, 0),
        [|[|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|]|]))) in
    assert_true(test_is_success(l_res))
    assert_equals_result(78, 306)
;;

(**fonction de test qui test si la direction du navire est bien vers le haut
    @author Sarah Favre
    @author Anne Celia
    @return unit
*)
let fonc_choose_direction_H() : unit =
    let l_res : (t_ship) t_test_result = test_exec(choose_direction, "applique la direction à un navire", (
        [|[|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|]|],{shit_type = CONTRE_TORPILLEUR ; x = 5 ; y = 5 ; direction = 0 ;size = 3 })) in
    assert_true(test_is_success l_res);
    assert_equals_result({shit_type = CONTRE_TORPILLEUR ; x = 5 ; y = 5 ; direction = 0 ;size = 3 })
;;

(**fonction de test qui test si la direction du navire est bien vers la droite
    @author Sarah Favre
    @author Anne Celia
    @return unit
*)
let fonc_choose_direction_D() : unit =
    let l_res : (t_ship) t_test_result = test_exec(choose_direction, "applique la direction à un navire", (
        [|[|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|]|],{shit_type = CONTRE_TORPILLEUR ; x = 5 ; y = 5 ; direction = 1 ;size = 3 })) in
    assert_true(test_is_success l_res);
    assert_equals_result({shit_type = CONTRE_TORPILLEUR ; x = 5 ; y = 5 ; direction = 1 ;size = 3 })
;;

(**fonction de test qui test si la direction du navire est bien vers le bas
    @author Sarah Favre
    @author Anne Celia
    @return unit
*)
let fonc_choose_direction_B() : unit =
    let l_res : (t_ship) t_test_result = test_exec(choose_direction, "applique la direction à un navire", (
        [|[|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|]|],{shit_type = CONTRE_TORPILLEUR ; x = 5 ; y = 5 ; direction = 2 ;size = 3 })) in
    assert_true(test_is_success l_res);
    assert_equals_result({shit_type = CONTRE_TORPILLEUR ; x = 5 ; y = 5 ; direction = 2 ;size = 3 })
;;

(**fonction de test qui test si la direction du navire est bien vers la gauche
    @author Sarah Favre
    @author Anne Celia
    @return unit
*)
let fonc_choose_direction_G() : unit =
    let l_res : (t_ship) t_test_result = test_exec(choose_direction, "applique la direction à un navire", (
        [|[|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|]|],{shit_type = CONTRE_TORPILLEUR ; x = 5 ; y = 5 ; direction = 3 ; size = 3 })) in
    assert_true(test_is_success l_res);
    assert_equals_result({shit_type = CONTRE_TORPILLEUR ; x = 5 ; y = 5 ; direction = 3 ;size = 3 })
;;

(**fonction de test qui test si la grille et la position retournées sont les bonnes apres un clic
    @author Sarah Favre
    @author Anne Celia
    @return unit
*)
let test_fonc_read_mouse(): unit =
let mouse_pos : (int*int)t_test_result = test_exec (read_mouse , "Retourne la bonne grille et la bonne position après un clic " , (126,258)) in
assert_true (test_is_success(mouse_pos));
assert_equals_result((3,4))mouse_pos
;;

(**fonction de test qui test si la liste de position horizontale renvoyé est la bonne 
    @author Sarah Favre
    @author Anne Celia
    @return unit
*)

let test_fonc_position_list_player_horizontal() : unit =
let l_res : (int * int)list t_test_result = test_exec(position_list_player,"Renvoie liste de position horizontale", {ship_type = CROISEUR ; x = 2; y = 2; direction = 1; size = 3;})in
assert_true(test_is_success l_res);
assert_equals_result([(2,2);(3,2);(4,2)],l_res)
;;

(**fonction de test qui test si la position aléatoire respecte les paramètres du jeu
    @author Sarah Favre
    @author Anne Celia
    @return unit
*)

let test_fonc_init_battleship(): unit =
let l_res: t_battleship t_test_result = test_exec (init_battleship,"Place un bateau aleatoirement",{ship_type=TORPILLEUR ; x=3;y=3;direction=3;size=2})in
assert_true (test_is_success l_res);
;;

(**fonction de test qui test si la souris est bien sur la grille du l'ordinateur
    @author Sarah Favre
    @author Anne Celia
    @return unit
*)

let test_fonc_which_grid_0(): int =
    let l_res : int t_test_result = test exec (wich_grid, "Conversion d'une grille en un chiffre",{margin = 30; cell_size = 15; grid_size = 24; window_width = 670;
    window_height = 390; message_size = 60} ) in
    assert_true (test_is_success(l_res));
    assert_equals_result {0, l_res}
;;

(**fonction de test qui test si la souris est bien sur la grille du joueur
    @author Sarah Favre
    @author Anne Celia
    @return unit
*)

let test_fonc_which_grid_1(): int =
    let l_res : int t_test_result = test exec (wich_grid, "Conversion d'une grille en un chiffre",{margin = 30; cell_size = 15; grid_size = 24; window_width = 670;
    window_height = 390; message_size = 60} ) in
    assert_true (test_is_success(l_res));
    assert_equals_result {1, l_res}
;;

(**fonction de test qui test si la souris est sur aucune des deux grilles
    @author Sarah Favre
    @author Anne Celia
    @return unit
*)

let test_fonc_which_grid_2(): int =
    let l_res : int t_test_result = test exec (wich_grid, "Conversion d'une grille en un chiffre",{margin = 30; cell_size = 15; grid_size = 24; window_width = 670;
    window_height = 390; message_size = 60} ) in
    assert_true (test_is_success(l_res));
    assert_equals_result {2, l_res}
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
    test_fonc_can_place_ship_haut_true();
    test_fonc_can_place_ship_droite_true();
    test_fonc_can_place_ship_bas_true();
    test_fonc_can_place_ship_gauche_true();
    test_fonc_can_place_ship_superposition();
    test_fonc_can_place_ship_sortie_haut();
    test_fonc_can_place_ship_sortie_droite();
    test_fonc_can_place_ship_sortie_bas();
    test_fonc_can_place_ship_sortie_gauche();
    test_report();
    test_reset_report();
    test_fonc_choose_direction_H();
    test_fonc_choose_direction_D();
    test_fonc_choose_direction_B();
    test_fonc_choose_direction_G();
    test_fonc_read_mouse();
    test_fonc_position_list_player_horizontal();
    test_fonc_init_battleship();
    test_fonc_which_grid_0();
    test_fonc_which_grid_1();
    test_fonc_which_grid_2();
;;


do_test() ;;
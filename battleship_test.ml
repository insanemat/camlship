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
let mouse_pos : int * (int*int)t_test_result = test_exec (read_mouse , "Retourne la bonne grille et la bonne position après un clic " , (126,258)) in
assert_true (test_is_success(mouse_pos));
assert_equals_result(0, (3,4))mouse_pos
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
    assert_equals_result (0,  l_res)
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
    assert_equals_result (1, l_res)
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
    assert_equals_result (2, l_res)
;;

(** fonction de test pour verifier si les bateaux se place correctement
@author Anne Celia
@author Marius Roumy
*)
let test_manual_placing_ships_list (): t_grid =
let l_res : (t_grid * t_ship_list) t_test_result = test exec ( manual_placing_ships_list , " Permet de placer l'ensemble des bateaux a placer dans sa grille"),(
[|[|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
          [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|]|], 
          
            [|{ship_type = PORTE_AVION; x = 0; y = 0; direction = 0; size = 5};
            {ship_type = CROISEUR; x = 0; y = 0; direction = 1; size = 4};
            {ship_type = CONTRE_TORPILLEUR; x = 0; y = 0; direction = 2; size = 3};
            {ship_type = CONTRE_TORPILLEUR; x = 0; y = 20; direction = 3; size = 3};
            {ship_type = TORPILLEUR; x = 0; y = 0; direction = 4; size = 2}|]) in   

assert_true(test_is_success(l_res))
 assert_equals_result ([|[|None ; None ; None ; None ; None; None; None ; None ; None ; None |];
 [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 1 ; direction = 0 ; size = 1} ; None; None; None ; None ; None|];
 [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 2 ; direction = 0 ; size = 2} ; None; None; None ; None ; None|];
 [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 3 ; direction = 0 ; size = 3} ; None; None; None ; None ; None|];
 [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 4 ; direction = 0 ; size = 4} ; None; None; None ; None ; None|];
 [|None ; None ; None ; None ; Some {ship_type = PORTE_AVION; x = 4; y = 5 ; direction = 0 ; size = 5} ; None; None; None ; None ; None|];
 [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
 [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
 [|None ; None ; None ; None ; None ; None ; None ; None ; None ; None|];
 [|None ; None ; {ship_type = CROISEUR; x = 0; y = 0; direction = 0; size = 5};
                 {ship_type = CROISEUR; x = 0; y = 0; direction = 1; size = 4};
                 {ship_type = CROISEUR; x = 0; y = 0; direction = 2; size = 3};
                 {ship_type = CROISEUR; x = 0; y = 20; direction = 3; size = 3};
                 {ship_type = CROISEUR; x = 0; y = 0; direction = 4; size = 2} None ; None ; None ; None ; None ; None ; None ; None|]|])
;;
 
(** permet de realiser tout les test d'un coup
@author Marius Roumy
@author Sarah Favre
@author Anne Celia
*)
let do_test(): unit =
    test_reset_report();
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
    test_report();
;;


do_test() ;;


(** Fonction auxiliaire pour créer une grille de test, utilisée dans les tests des fonctions de tir.
    @return : une grille vide avec des coordonnées initialisées pour la grille de l'ordinateur.
    @author Maël ICAPI
*)
let create_test_grid () : t_grid =
    let params = init_params () in
    let grid = Array.make_matrix 10 10 {x = 0; y = 0; ship = None; stat = UNKNOWN} in
    for i = 0 to 9 do
      for j = 0 to 9 do
        let offset_x = params.margin in
        let offset_y = params.margin + params.message_size in
        let px = offset_x + i * params.grid_size in
        let py = offset_y + j * params.grid_size in
        grid.(i).(j) <- {x = px; y = py; ship = None; stat = UNKNOWN}
      done
    done;
    grid
  
  (** Place un bateau sur la grille à une position donnée, utilisée dans les tests des fonctions de tir.
      @param grid : la grille où placer le bateau.
      @param ship : le bateau à placer.
      @return : unit (modifie la grille en place).
      @author Maël ICAPI
  *)
  let place_ship (grid : t_grid) (ship : t_ship) : unit =
    let positions = positions_list ship in
    (* On parcourt la liste des positions avec List.iter *)
    List.iter (fun pos ->
      let x = fst pos in
      let y = snd pos in
      let cell = grid.(y).(x) in
      grid.(y).(x) <- { cell with ship = Some ship; stat = PLAYER }
    ) positions
  
  (** Simule un clic sur la grille de l'ordinateur et appelle find_ship, pour les tests.
      @param grid : la grille où effectuer le clic.
      @param mouse_x : coordonnée x du clic.
      @param mouse_y : coordonnée y du clic.
      @return : la liste des bateaux trouvés par find_ship.
      @author Maël ICAPI
  *)
  let simulate_find_ship (grid : t_grid) (mouse_x : int) (mouse_y : int) : t_ship list =
    let params = init_params () in
    (* Vérifier si le clic est dans la grille *)
    let in_grid = mouse_x >= params.margin && mouse_x < params.margin + params.grid_size * 10 &&
                  mouse_y >= params.margin + params.message_size &&
                  mouse_y < params.margin + params.message_size + params.grid_size * 10 in
    if not in_grid then
      []  (* Clic hors grille *)
    else
      let offset_x = params.margin in
      let offset_y = params.margin + params.message_size in
      let grid_x = (mouse_x - offset_x) / params.grid_size in
      let grid_y = (mouse_y - offset_y) / params.grid_size in
      (* Vérifier si les indices sont valides *)
      if grid_x < 0 || grid_x >= 10 || grid_y < 0 || grid_y >= 10 then
        []  (* Indices hors grille *)
      else
        (* On suppose que find_ship utilise ces indices pour chercher un bateau *)
        find_ship grid
  
  (** Simule un clic sur la grille de l'ordinateur et appelle player_shoot, pour les tests.
      @param grid : la grille où effectuer le tir.
      @param mouse_x : coordonnée x du clic.
      @param mouse_y : coordonnée y du clic.
      @return : la grille mise à jour après le tir.
      @author Maël ICAPI
  *)
  let simulate_player_shoot (grid : t_grid) (mouse_x : int) (mouse_y : int) : t_grid =
    let params = init_params () in
    (* Vérifier si le clic est dans la grille *)
    let in_grid = mouse_x >= params.margin && mouse_x < params.margin + params.grid_size * 10 &&
                  mouse_y >= params.margin + params.message_size &&
                  mouse_y < params.margin + params.message_size + params.grid_size * 10 in
    if not in_grid then
      grid  (* Clic hors grille *)
    else
      let offset_x = params.margin in
      let offset_y = params.margin + params.message_size in
      let grid_x = (mouse_x - offset_x) / params.grid_size in
      let grid_y = (mouse_y - offset_y) / params.grid_size in
      (* Vérifier si les indices sont valides *)
      if grid_x < 0 || grid_x >= 10 || grid_y < 0 || grid_y >= 10 then
        grid  (* Indices hors grille *)
      else
        (* On suppose que player_shoot utilise ces indices pour effectuer un tir *)
        player_shoot grid
  
  (** Teste la fonction [find_ship] avec différents cas de clics simulés.
      Fait partie d'une suite de tests dans un fichier de test plus large.
      @author Maël ICAPI
  *)
  let test_find_ship () =
    let grid = create_test_grid () in
    let ship = { ship_type = PORTE_AVION; x = 2; y = 2; direction = 1; size = 5 } in
    place_ship grid ship;
  
    (* Test 1 : Clic sur une case avec un bateau *)
    let res1 = test_exec (simulate_find_ship, "find_ship - clic sur bateau", (grid, 78, 138)) in
    (* 78 = 30 + 2 * 24, 138 = 90 + 2 * 24, donc (2, 2) *)
    assert_equals_result_m ("Devrait trouver un bateau", [ship], res1);
  
    (* Test 2 : Clic sur une case vide *)
    let res2 = test_exec (simulate_find_ship, "find_ship - clic sur case vide", (grid, 30, 90)) in
    (* (0, 0) *)
    assert_equals_result_m ("Ne devrait pas trouver de bateau", [], res2);
  
    (* Test 3 : Clic hors grille *)
    let res3 = test_exec (simulate_find_ship, "find_ship - clic hors grille", (grid, 500, 500)) in
    assert_equals_result_m ("Clic hors grille, liste vide", [], res3)
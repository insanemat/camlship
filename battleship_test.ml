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

test_fonc_init_params ();;
test_report();;
test_reset_report();;




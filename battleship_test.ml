#use "CPtest.ml";;
#use "battleship.ml";;

(** Vérifie si la marge est égale à 30 pixels 
    @author Anne Celia Mensah 
    @author Maël ICAPI
    @return unit
*)
let test_fonc_1_init_params() : unit =
    let l_res : t_params t_test_result = test_exec (init_params, "paramètre bien initialisé", ()) in
    assert_true(test_is_success(l_res));
    assert_equals_result ({margin = 30 ; cell_size = 15 ; grid_size = 10 ; window_width = 1000 ; window_height = 1000; message_size = 60 } , l_res)
;;
test_fonc_1_init_params ();;
test_report();;
test_reset_report();;



let test_fonc_1_init_params() : unit =
    let l_res : t_params t_test_result = test_exec (init_params, "paramètre mal initialisé", ()) in
    assert_true(test_is_success(l_res));
    assert_equals_result ({margin = 30 ; cell_size = 15 ; grid_size = 10 ; window_width = 1000 ; window_height = 1000; message_size = 60 } , l_res)
;;
test_fonc_1_init_params ();;
test_report();;
test_reset_report();;




(** Vérifie si la grille est égale à 15 
    @author Anne Celia Mensah 
    @author Maël ICAPI
    @return unit
*)
let test_fonc_1_init_params_cell_size () : unit =
    let l_res : t_params t_test_result = test_exec (init_params, "Taille d'une case à 15 pixels", ()) in
    assert_equals (l_res.cell_size, 15)
;;

(** Vérifie si la hauteur de la zone est égale à 60 pixels matérialisés en bleu 
    @author Anne Celia Mensah 
    @author Maël ICAPI
    @return unit
*)
let test_fonc_1_init_params_message_size () : unit = 
    let l_res : t_params t_test_result = test_exec ( init_params , " La hauteur de la zonz 60 pixelss", ())in
assert_equals ( l_res.message_size, 60)
;;

(** Vérifie si les 2 grilles ont 10 cases 
    @author Anne Celia Mensah 
    @author Maël ICAPI
    @return unit
*)
let test_fonc_1_init_params_grid_size () : unit = 
    let l_res : t_params t_test_result = test_exec ( init_params , "Deux grilles de 10 cases",())in
assert_equals( l_res.grid_size,10)
;;

(** Vérifie si la fenêtre est bien ouverte 
    @author Anne Celia Mensah 
    @author Maël ICAPI
    @return unit
*)
let test_fonc_battleship_game_open_graph () : unit =
;;

(** Faut-il tester le nom de la fonction ? 
    @author Anne Celia Mensah 
    @author Maël ICAPI
    @return unit
*)
let test_fonc_battleship_set_window_title () : unit = ()
;;

(** Vérifie si la fenêtre est de la bonne taille (X) 
    @author Anne Celia Mensah 
    @author Maël ICAPI 
    @return unit
*)
let test_fonc_battleship_size_x () : unit =
   let l_res: t_params t_test_result = test_exec ( init_params , "Dimmension x",()) in
   assert_equals ( l_res.size_x,670 )
;;

(** Vérifie si la fenêtre est de la bonne taille (Y) 
    @author Anne Celia Mensah 
    @author Maël ICAPI 
    @return unit
*)
let test_fonc_battleship_size_y () : unit = 
   let l_res : t_params t_test_result = test_exec ( init_params , "Dimension y ",()) in 
   assert_equals( l_res.size_y,390)
;;

#use "CPtest.ml";;
#use "battleship.ml";;

(** Vérifie si la marge est égale à 30 pixels 
    @author Anne Celia Mensah 
    @author Maël ICAPI
    @return unit
*)
let test_fonc_1_init_params_margin () : unit =
    let l_res : t_params t_test_result = test_exec (init_params (), "Marge à 30 pixels", (30, 0, 0, 0, 0, 0)) in
    assert_equals (l_res.margin, 30)
;;


(** Vérifie si la grille est égale à 15 
    @author Anne Celia Mensah 
    @author Maël ICAPI
    @return unit
*)
let test_fonc_1_init_params_cell_size () : unit =
    let l_res : t_params t_test_result = test_exec (init_params (), "Taille d'une case à 15 pixels", (30, 15, 0, 0, 0, 0)) in
    assert_equals (l_res.cell_size, 15)
;;

(** Vérifie si la hauteur de la zone est égale à 60 pixels matérialisés en bleu 
    @author Anne Celia Mensah 
    @author Maël ICAPI
    @return unit
*)
let test_fonc_1_init_params_message_size () : unit = ()
;;

(** Vérifie si les 2 grilles ont 10 cases 
    @author Anne Celia Mensah 
    @author Maël ICAPI
    @return unit
*)
let test_fonc_1_init_params_grid_size () : unit = ()
;;

(** Vérifie si la fenêtre est bien ouverte 
    @author Anne Celia Mensah 
    @author Maël ICAPI
    @return unit
*)
let test_fonc_battleship_game_open_graph () : unit = ()
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
let test_fonc_battleship_size_x () : unit = ()
;;

(** Vérifie si la fenêtre est de la bonne taille (Y) 
    @author Anne Celia Mensah 
    @author Maël ICAPI 
    @return unit
*)
let test_fonc_battleship_size_y () : unit = ()
;;


(** Ce fichier est notre fichier source 
@author Sarah Favre
@author Marius Roumy
@author Anne-Celia Mensah
@author Maêl Icapi

@version 1.0
*)

(** Le type t_params est le type structuré pour nos paramètre
@author Sarah Favre
@author Marius Roumy
*)
type t_params = {margin : int ; cell_size : int ; grid_size : int ; window_width : int ; window_height : int ; message_size : int} 
(*margin = marge autour des grilles ; cell_size = taille des cases ; grid_size = nombres de cases par côté
; window_width = largeur totale de la fenêtre ; window_height = hauteur totale de la fenêtre ; message_size = taille des caractères *)
;;

(** la fonction initialise nos paramètres
@author Sarah Favre
@author Marius Roumy
@return t_params
*)
let init_params(): t_params =
     {margin = 30 ; cell_size = 15 ; grid_size = 10 ; window_width = 1000 ; window_height = 1000; message_size = 60 }
;;

(**la fonction permet d'afficher les deux grilles vides et le nom des deux joueurs
@author Sarah Favre
@author Marius Roumy
@param p_params nos paramètres 
@return unit
*)
let display_empty_grids(p_params : t_params): unit =
()
;;

(**la fonction permet d'ouvrir la fenètre graphique aux dimensions appropriées, mettre a jour son titre
et effectuer les affichages adéquates.
@author Sarah Favre
@author Marius Roumy
@param p_margin marge autour des grilles
@param p_cell_size taille des cases
@param p_grid_size nombres de cases par côté
@param p_window_width largeur totale de la fenêtre
@param p_window_height hauteur totale de la fenêtre
@param p_message_size taille des caractères
@return unit
*)
let battleship_game(p_margin, p_cell_size, p_grid_size, p_window_width, p_window_height, p_message_size : int * int * int * int * int * int): unit =
()
;;

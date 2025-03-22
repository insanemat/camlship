(** Ce fichier est notre fichier source 
@author Sarah Favre
@author Marius Roumy
@author Anne-Celia Mensah
@author Maêl Icapi

@version 1.0
*)

#use "CPgraphics.ml" ;;

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
   let p_params : t_params = {margin = 30 ; cell_size = 15 ; grid_size = 10 ; window_width = 670 ; window_height = 390; message_size = 60 } in
   p_params
;;

(**la fonction permet d'afficher les deux grilles vides et le nom des deux joueurs
@author Sarah Favre
@author Marius Roumy
@author Maêl Icapi
@param p_params nos paramètres 
@return unit
*)
let display_empty_grids (p_params : t_params) : unit =
     open_graph (p_params.window_width, p_params.window_height); (* Fenêtre d'origine *)
   
     draw_rect (
       p_params.margin, 
       p_params.margin, 
       p_params.window_width - (2 * p_params.margin), 
       p_params.window_height - (2 * p_params.margin)
     ); (* Fenêtre principale, en rouge sur le PDF, contenant toutes les autres fenêtres du jeu *)
   
     draw_rect (
       (p_params.window_width / 2) - (p_params.margin / 2),  
       p_params.margin + p_params.message_size + p_params.cell_size, 
       p_params.margin,  
       p_params.window_height - 2 * p_params.margin - 2 * p_params.cell_size - p_params.message_size
     ); (* Rectangle qui sépare les deux grilles de jeu, en rouge sur le PDF *)
   
     draw_rect (
       p_params.margin,  
       p_params.margin, 
       p_params.window_width - 2 * p_params.margin,  
       p_params.message_size
     ); (* Rectangle contenant les messages du jeu, en bleu sur le PDF *)
   
     draw_rect (
       p_params.margin, 
       p_params.margin + p_params.message_size - p_params.cell_size,  
       p_params.window_width - 2 * p_params.margin, 
       p_params.cell_size
     ); (* Rectangle vert contenu dans le rectangle bleu *)
   
     draw_rect (
       p_params.margin,  
       p_params.window_height - p_params.margin - p_params.cell_size, 
       p_params.window_width - 2 * p_params.margin,  
       p_params.cell_size  
     );(*Rectangle vert collé au coté supérieur du rectangle principal rouge*)
   
     draw_rect (
       p_params.margin,
       p_params.margin + p_params.message_size,
       p_params.cell_size,
       (p_params.cell_size) * 16
     ); (* Rectangle des chiffres du côté gauche *)
   
     draw_rect (
       p_params.margin,
       p_params.margin + p_params.message_size + (p_params.cell_size) * 16,
       (p_params.cell_size) * 19,
       p_params.cell_size
     ); (* Rectangle des lettres du côté gauche *)
   
     (* Rectangle des chiffres du côté droit *)
     draw_rect (
       (p_params.window_width / 2) + (p_params.margin / 2), 
       p_params.margin + p_params.message_size,  
       p_params.cell_size,  
       (p_params.cell_size) * 16  
     );
   
     (* Rectangle des chiffres du côté gauche *)
     draw_rect (
       (p_params.window_width / 2) + (p_params.margin / 2),  
       p_params.margin + p_params.message_size + (p_params.cell_size) * 16,  
       (p_params.cell_size) * 19,  
       p_params.cell_size  
     );

     
;;
   
let test_graph(p_params : t_params): unit =
for i = 1 to 10 do
  for j = 1 to 10 do 
draw_rect(
  p_params.margin + (p_params.cell_size + 9 )*i,
  p_params.margin + p_params.message_size,
  p_params.cell_size + 9 ,
  (p_params.cell_size + 9) * j
);
  done;
done;
;;
test_graph(init_params());;

display_empty_grids(init_params()) ;;
close_graph();;

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

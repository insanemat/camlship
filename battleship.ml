(** Ce fichier est notre fichier source 
@author Sarah Favre
@author Marius Roumy
@author Anne-Celia Mensah
@author Maël Icapi

@version 1.0
*)
#use "topfind";;
#require "graphics";;
#require "unix";;
open Random;;

#mod_use "CPgraphics.ml" ;;
open CPgraphics;;
(**
Détermine le type de bateau    
*)
type t_ship_type = PORTE_AVION | CROISEUR | CONTRE_TORPILLEUR | TORPILLEUR;;

(**
Représentation d'un bateau : son type, la position de la première case en x et y, sa direction entre 1(horizon) et 2(vertical) et sa taille   
*)
type t_ship = {ship_type : t_ship_type; x : int ; y : int ; direction : int ; size : int} ;;

(**
Détermine la taille de allouée à chaque type de bateau
*)
type t_ship_size = t_ship_type * int;;

(*Représente la postition graphique d'une cellule et le bateau qu'elle contient*)
type t_cell = {x : int; y : int; ship : t_ship option} ;;

(**
Matrice représentant une grille grille   
*)
type t_grid = t_cell array array;;


(** Le type t_params est le type structuré pour nos paramètre
@author Sarah Favre
@author Marius Roumy
*)
type t_params = {margin : int ; cell_size : int ; grid_size : int ; window_width : int ; window_height : int ; message_size : int ; ship_size : t_ship_size list} 
(*margin = marge autour des grilles ; cell_size = taille des cases ; grid_size = nombres de cases par côté
; window_width = largeur totale de la fenêtre ; window_height = hauteur totale de la fenêtre ; message_size = taille des caractères *)
;;

(** la fonction initialise nos paramètres
@author Sarah Favre
@author Marius Roumy
@return t_params
*)
let init_params(): t_params =
   let p_params : t_params = {margin = 30 ; cell_size = 15 ; grid_size = 24 ; window_width = 670 ; window_height = 390; message_size = 60 ; ship_size = [(PORTE_AVION, 5);
   (CONTRE_TORPILLEUR, 3); (CONTRE_TORPILLEUR, 3); (TORPILLEUR, 2)] } in
   p_params
;;

(**la fonction permet d'afficher les deux grilles vides et le nom des deux joueurs
@author Sarah Favre
@author Marius Roumy
@author Maël Icapi
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
       p_params.margin + p_params.message_size, 
       p_params.margin,  
       (p_params.window_height - 2 * p_params.margin - 2 * p_params.cell_size - p_params.message_size) + p_params.cell_size
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
       ((p_params.cell_size) * 19) + 5,
       p_params.cell_size
     ); (* Rectangle des lettres du côté gauche *)
   
     (* Rectangle des chiffres du côté droit *)
     draw_rect (
       (p_params.window_width / 2) + (p_params.margin / 2), 
       p_params.margin + p_params.message_size,  
       p_params.cell_size,  
       (p_params.cell_size) * 16  
     );
   
     (* Rectangle des lettres du côté gauche *)
     draw_rect (
       (p_params.window_width / 2) + (p_params.margin / 2),  
       p_params.margin + p_params.message_size + (p_params.cell_size) * 16,  
       ((p_params.cell_size) * 19) + 5,  
       p_params.cell_size  
     );

     (* Grille de gauche*)
     for i = 1 to 10 do
      for j = 1 to 10 do 
    draw_rect( 
      p_params.margin + p_params.grid_size * i,
      p_params.margin + p_params.message_size,
      p_params.grid_size,
      p_params.grid_size * j
    )
    ;
      done;
    done;

    (* Grille de droite*)
    for i = 1 to 10 do
      for j = 1 to 10 do
        draw_rect(
          ((p_params.window_width / 2) - (p_params.margin / 2) + p_params.margin) + p_params.grid_size * i,
          p_params.margin + p_params.message_size,
          p_params.grid_size,
         p_params.grid_size * j
    )
    ;
      done;
    done; 
;;

(**
  
  Génère une position aléatoire pour un bateau donné.
  @param p_ship Le bateau à positionner.
  @return Un nouveau bateau avec une position et direction aléatoire.
*)
let generate_random_position(p_ship : t_ship) : t_ship =
let new_ship : t_ship = {ship_type = p_ship.ship_type; x = Random.int(10); y = Random.int(10); direction = Random.int(2); size = p_ship.size} in 
 new_ship
;;

(**
  Calcule la liste des positions qu'un bateau occupe sur la grille.
  @author Maël Icapi
  @param ship Le bateau dont on veut obtenir les positions.
  @return Une liste de positions représentant le bateau sur la grille.
*)
let rec positions_list(ship : t_ship) : (int * int) list = 
  if ship.size <= 0 then []
  else
    let (x, y) = (ship.x, ship.y) in
    let next_ship =
      match ship.direction with
      | 0 -> { ship with y = y + 1; size = ship.size - 1 }  (* Haut *)
      | 1 -> { ship with x = x + 1; size = ship.size - 1 }  (* Droite *)
      | 2 -> { ship with y = y - 1; size = ship.size - 1 }  (* Bas *)
      | _ -> { ship with x = x - 1; size = ship.size - 1 }  (* Gauche *)
    in
    (x, y) :: positions_list(next_ship)    
;;

(**
  Vérifie si un bateau peut être placé sur la grille sans chevauchement.
  @author Maël Icapi
  @param p_current_grid La grille actuelle avec les bateaux déjà placés.
  @param p_ship_to_place La liste des positions du bateau à placer.
  @return true si le bateau peut être placé, false sinon.
*)
let can_place_ship(p_current_grid, p_ship_to_place : t_grid * t_ship) : bool =
  let p_positions = positions_list(p_ship_to_place) in
  List.for_all (fun (x, y) -> 
  x >= 0 && x < Array.length p_current_grid.(0) &&
  y >= 0 && y < Array.length p_current_grid &&
  (p_current_grid.(y).(x)).ship = None) p_positions
;;

(** 
  Place automatiquement tous les bateaux dans la grille en respectant les règles.
  @author Maël Icapi
  @param p_grid La grille initiale.
  @return Une grille avec tous les bateaux placés.
*)
let rec auto_placing_ships (p_grid, p_ship_list_to_place : t_grid * t_ship list) : t_grid = 
  if p_ship_list_to_place = [] then p_grid  
  else 
    let p_current_ship = generate_random_position (List.hd p_ship_list_to_place) in
    if can_place_ship (p_grid, p_current_ship) then (
      let p_positions = positions_list p_current_ship in
      List.iter (fun (x, y) ->
        let old_cell = p_grid.(y).(x) in
        p_grid.(y).(x) <- { old_cell with ship = Some p_current_ship }
      ) p_positions;
      auto_placing_ships (p_grid, List.tl p_ship_list_to_place)
    )
    else
      auto_placing_ships (p_grid, p_ship_list_to_place)
;;

(**
   Permet de colorier une cellule de la grille dans un couleur donnée
   @param p_x voir p_y
   @param p_y la coordonnée sur l'axe y du point inférieur gauche
   @param p_params les paramètres de la partie
   @param p_color la couleur désirée
   @author Anne Celia Mensah 
*)
let color_cell(p_x, p_y, p_params, p_color : int * int * t_params * Graphics.color) : unit =
  set_color(p_color);
  fill_rect(p_x, p_y, p_params.grid_size, p_params.grid_size);
;;

(** [cell_to_pixel(p_cell, p_params)] convertit une cellule logique de la grille en coordonnées
    de pixel à afficher à l’écran. Elle utilise les paramètres d’affichage pour ajuster
    la position selon la taille de la grille, les marges, etc.

    @param p_cell : la cellule de type [t_cell] dont on veut obtenir la position graphique.
    @param p_params : les paramètres graphiques (marge, taille des cases, etc.).
    @return un couple d’entiers (x, y) correspondant aux coordonnées en pixels.
    @author Maël Icapi
*)
let cell_to_pixel(p_cell, p_params : t_cell * t_params) : int * int = 
  let x : int = p_cell.x + p_params.margin + p_params.grid_size and
      y : int = p_cell.y + p_params.margin + p_params.message_size in 
  x,y
;;

(** [display_grid p_grid] affiche les cases contenant un bateau sur une grille graphique.
    Chaque cellule de la grille est parcourue, et si un bateau est présent (champ [ship] ≠ None),
    la cellule est coloriée à l’aide de [color_cell].

    @param p_grid : la grille de type [t_grid] à afficher.
    @return unit (effet de bord : dessine à l’écran).
    @author Maël Icapi
*)
let display_grid (p_grid : t_grid) : unit =
  for i = 0 to Array.length p_grid - 1 do
    for j = 0 to Array.length p_grid.(i) - 1 do
      if p_grid.(i).(j).ship <> None then
        let (px, py) = cell_to_pixel(p_grid.(i).(j), init_params()) in
        color_cell(px, py, init_params(), black)
    done
  done
;;

(** [create_computer_grid p_params] crée et initialise la grille de l’ordinateur :
    - Elle alloue une grille 10x10 vide (sans bateaux),
    - Place des coordonnées graphiques dans chaque cellule,
    - Génère et place automatiquement une liste prédéfinie de bateaux.

    @param p_params : les paramètres graphiques nécessaires à l’affichage.
    @return une grille [t_grid] avec des bateaux placés aléatoirement.
    @author Maël Icapi
*)
let create_computer_grid (p_params : t_params) : t_grid =
  let p_grid = Array.make_matrix 10 10 {x = 0; y = 0; ship = None} in
  for i = 1 to 9 do
    for j = 1 to 9 do
      p_grid.(i).(j) <- {x = i * 24; y = j * 24; ship = None}
    done
  done;
  let ships_to_place = [
    {ship_type = PORTE_AVION; x = 0; y = 0; direction = 0; size = 5};
    {ship_type = CROISEUR; x = 0; y = 0; direction = 0; size = 4};
    {ship_type = CONTRE_TORPILLEUR; x = 0; y = 0; direction = 0; size = 3}
  ] in
  let final_grid = auto_placing_ships (p_grid, ships_to_place) in
  final_grid
;;


(**la fonction permet d'ouvrir la fenètre graphique aux dimensions appropriées, mettre a jour son titre
et effectuer les affichages adéquates.
@author Sarah Favre
@author Marius Roumy
@return unit
*)



let battleship_game(): unit =
let l_params : t_params = init_params() in
display_empty_grids(l_params)
;;
 
battleship_game();;
display_grid(create_computer_grid(init_params()));;
close_graph()
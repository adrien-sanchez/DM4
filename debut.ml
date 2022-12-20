(*le type image : une matrice d'entiers.
  Les entiers sont censés être entre 0 et 255
  uint8_t serait d'ailleurs mieux que int
*)
type img = int array array;;

(*Q0 O(1)*)
let dim mat = (*calcule les dimensions d'une image*)
  Array.length mat.(0), Array.length mat;;

(*valeur absolue des flottants*)
let abs x = if x > 0.0 then x else -. x;;

(*pour chronometrer le temps d'exécution d' une fonction
 f : une fonction à un paramètre
 x : un paramètre *)
let time f x =
    let t = Sys.time() in
    let fx = f x in
    Printf.printf "execution time: %fs\n" (Sys.time() -. t);
    fx

(* charge une image quelconque (.jpg,.png... comme supporté par
   camlimages) vers une matrice d'entiers entre 0 et 255 :
   int*array*array. Nous appelons une telle matrice une 
   "image en niveau de gris" *)
let load_matrix (name:string) =
  let img = Images.load name [] in
  let gimg = Graphic_image.array_of_image img in
  let graylevel color = color mod 256
  in
  Array.map (Array.map graylevel) gimg;;



(*Le module Graphics affiche des images au format RVB.
 Une image (pour nous) est un int*array*array.
 Nous indiquons à Graphics que R,V,B sont égaux à R*)
let to_graphics (mat:img) =
  Graphics.make_image
    (Array.map
       (Array.map
          (fun x -> Graphics.rgb x x x))
       mat)



(*name : nom de fichier photo en niveau de gris
  f : une fonction de transformation d'image
  La fonction charge la photo et met le contenu 
  dans une img *)
let display (name:string) (f:img->img):unit =
  let mat = load_matrix name (*image initiale*) in
  let w,h = dim mat in
  (*on calcule l'image transformée (newmat)
    et on affiche le temps d'exécution : *)
  let newmat = time f mat in
  let sw = string_of_int w and sh = string_of_int h in
  let dimensions = " "^sw^"x"^sh in
  Graphics.open_graph dimensions;
  (* dessine l'image*)
  Graphics.draw_image (to_graphics newmat) 0 0;
  Unix.sleep 5;(*attendre 5 secondes*)
  Graphics.close_graph () (*fermer la fenêtre*);;



(*Q0 O(wh)*)
let negative (mat:img):img =
  (*retourne le négatif d'une image*)
  let w,h =  dim mat (*O(1)*) in
  let newmat = Array.make_matrix h w 0 in
  for i=0 to h-1 do
    for j = 0 to w-1 do
      newmat.(i).(j) <- 255- mat.(i).(j);
    done;
  done;
  newmat;;


(*lance l'affichage à partir de la ligne de commande et de la fonction
   appliquée à la matrice*)
let () =
  display  Sys.argv.(1) negative;;


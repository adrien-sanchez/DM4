(*le type image : une matrice d'entiers.
  Les entiers sont censés être entre 0 et 255
  uint8_t serait d'ailleurs mieux que int
*)

(*ocamlfind ocamlc -o test -package graphics -package unix -package camlimages.png -package camlimages.graphics -linkpkg debut.ml*)
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

  let seuil threshold image =

    let w, h = dim image in
    let final = Array.make_matrix w h 0 in
  
    for j = 0 to (w - 1) do
      for i = 0 to (h - 1) do
        if (image.(i).(j) > threshold) then final.(j).(i) <- 255 
        else final.(j).(i) <- 0;(*Printf.printf "i:%d, j:%d"i j *)
      done;
    done;(*358 478*)
    
    final;;
    let symh image =
  
      let w, h = dim image in
      let final = Array.make_matrix h w 0 in
      
      for i = (h-1) downto 0 do
        for j = (w-1) downto 0 do
     final.((h-1)-i).(j) <- image.(i).(j) (*Printf.printf "i:%d, j:%d" wf hf *)
        done;
      done;(*690 460*)
      final
    ;;
    let symv image =

      let w, h = dim image in
      let final = Array.make_matrix h w 0 in
    
      for i = 0 to (h - 1) do
        for j = (w-1) downto 0 do  
          final.(i).((w-1)-j) <- image.(i).(j)
        done;
      done; 
      final
    ;;
    (*Q 2*)
    let reduction_moitie_ligne array =
    
    let length = ((Array.length array)/2) in
    let final = Array.make length 0 in
      
    let i = ref 0 in
      
    while (!i <= length) do 
      final.((!i)/2) <- ((array.(!i) + array.(!i + 1))/2);
      i := !i + 2;
    done;
    final
  ;;
  let reduction_moitie_image image =

    let w, h = dim image in
  
    for i = 0 to (h - 1) do
      image.(i) <- reduction_moitie_ligne image.(i)
    done;
    
  ;;
  let getWestLevel i j image =
    if(i-1 < 0) then image.(i).(j)
    else image.(i-1).(j)
  ;;
  
  let getNorthLevel i j image =
    let w_max, h_max = dim image in
    if(j+1 > h_max-1) then image.(i).(j)
    else image.(i).(j+1)
  ;;
  
  let getEastLevel i j image =
    let w_max, h_max = dim image in
    if(i+1 > w_max-1) then image.(i).(j)
    else image.(i+1).(j)
  ;;
  
  let getSouthLevel i j image =
    if(j-1 < 0) then image.(i).(j)
    else image.(i).(j-1)
  ;;
  
  let getPixelEnergy i j image =
    (abs_float(((getEastLevel i j image) -. (getWestLevel i j image))/.2.0) +. (abs_float(((getNorthLevel i j image) -. (getSouthLevel i j image))/.2.0)))
  ;;
  let energie image = (*probléme car le prgm considére que image est un int array array donc incompatible*)
    let w, h = dim image in
    let energy = Array.make_matrix w h 0.0 in
  
    for i = 0 to (h - 1) do
      for j = 0 to (w - 1) do
        energy.(i).(j) <- (getPixelEnergy i j image);
      done;
    done;
    energy
  ;;  
  (*Q 6*)
  let enlever array i = 

    let length = Array.length array in
    if(i > length-1 || i < 0) then array
    else
      let final = (Array.make (length-1) 0) in 
  
      for k = 0 to (length-1) do 
        if(k < i) then final.(k) <- array.(k)
        else if(k > i) then final.(k-1) <- array.(k)
      done;
      final
  ;;
  (*Q 7*)
  let indice_min array =
  
    let length = Array.length array in
    if(length == 0) then 0
    else
      let current = ref 0 in
    
      for i = 0 to (length-1) do 
        if(array.(i) < array.(!current)) then
          current := i
      done;
      !current
  ;;
  
let () =
  
  display  Sys.argv.(1) symv;;
 


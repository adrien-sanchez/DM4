type img = int array array;;
type energy = float array array;;

let foi = float_of_int;;
let iof = int_of_float;;

let dim mat =
  Array.length mat.(0), Array.length mat;; 

let load_matrix (name:string) =
  let img = Images.load name [] in
  let gimg = Graphic_image.array_of_image img in
  let graylevel color = color mod 256
  in
  Array.map (Array.map graylevel) gimg;;

(*https://stackoverflow.com/questions/9061421/running-time-in-ocaml*)

let time f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  fx

(*Q.1*)

let seuil threshold image =

  let w, h = dim image in
  let final = Array.make_matrix w h 0 in

  for i = 0 to (h - 1) do
    for j = 0 to (w - 1) do
      if (image.(i).(j) > threshold) then final.(i).(j) <- 255;
    done;
  done;
  final;;

(*Q.1*)

let symh image =
  
  let w, h = dim image in
  let final = Array.make_matrix w h 0 in

  for i = (h-1) downto 0 do
    for j = (w-1) downto 0 do
      final.((h-1)-i).((w-1)-j) <- image.(i).(j)
    done;
  done;
  final
;;

(*Q.1*)

let symv image =

  let w, h = dim image in
  let final = Array.make_matrix w h 0 in

  for i = 0 to (h - 1) do
    for j = (w-1) downto 0 do
      final.(i).((w-1)-j) <- image.(i).(j)
    done;
  done; 
  final
;;

(*Q.2*)

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

(*Q.3*)

(*Complexité au pire : C(w, h) = 2h*[(w + (8)(w/2) + 3] + 4 = 2h*(5w + 3) + 4*)

let reduction_moitie_image image =

  let w, h = dim image in

  for i = 0 to (h - 1) do
    image.(i) <- reduction_moitie_ligne image.(i)
  done
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

(*Q.4*)

let energie image = 
  let w, h = dim image in
  let energy = Array.make_matrix w h 0.0 in

  for i = 0 to (h - 1) do
    for j = 0 to (w - 1) do
      energy.(i).(j) <- (getPixelEnergy i j image);
    done;
  done;
  energy
;;

(*Q.5*)

let energie_to_image energy = 

  let w, h = dim energy in
  let final = Array.make_matrix w h 0 in

  for i = 0 to (h - 1) do
    for j = 0 to (w - 1) do
      final.(i).(j) <- (iof energy.(i).(j));
    done;
  done;
;;

(*Q.6*)

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

(*Q.7*)

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

(*Q.8*)

let reduction_ligne_par_ligne image =

  let w, h = dim image in

  let energy = energie image in
  let new_energy = Array.make_matrix w (h-1) 0.0 in

  for i = 0 to (h - 1) do
    new_energy.(i) <- enlever (energy.(i)) (indice_min (energy.(i)))
  done;
  image = energie_to_image new_energy
;;

(*Q.9*)

(*Complexité au pire : *)

(*On defini l'energie d'un pixel grâce à la nuance
  de gris des 4 pixels autour de ce dernier; ainsi le déplacement
  de pixels observé provient du fait que les pixels encadrant le pixel
  de faible energie sont succeptibles d'avoir eux aussi une faible energie. (à revoir)*)

let itere_reduction_ligne_par_ligne image n =

  let i = ref 0 in

  while (!i < n) do
    reduction_ligne_par_ligne image;
    i := !i + 1;
  done;
  image;;

(*Q.10*)

let meilleure_colonne e =

  let buffer = ref 0 in
  let index = ref 0 in
  
  let w, h = dim e in

  for j = 0 to (w - 1) do
    let sum = ref 0 in
    for i = 0 to (h - 1) do
      sum := !sum + e.(i).(j)
    done;
    if(!sum < !buffer) then
      index := j; buffer := !sum;
  done;
  !index
;;

(*Q.11*)

let reduction_meilleure_colonne image =

  let energy = energie image in
  let colonne = meilleure_colonne energy in

  for i=0 to (h-1) do
    image.(i) <- enlever (image.(i) meilleure_colonne);
  done;

(*Q.12*)

let itere_meilleure_colonne image n =
  for (i=0) to n do
    reduction_meilleure_colonne image;
  done;
image;
;;
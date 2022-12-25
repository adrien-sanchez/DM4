type img = int array array;;

let dim mat =
  Array.length mat.(0), Array.length mat;;

let load_matrix (name:string) =
  let img = Images.load name [] in
  let gimg = Graphic_image.array_of_image img in
  let graylevel color = color mod 256
  in
  Array.map (Array.map graylevel) gimg;;

let seuil threshold image =

  let w, h = dim image in
  let final = Array.make_matrix w h 0 in

  for (i = 0) to (h - 1) do
    for (j = 0) to (w - 1) do
      if (image.(i).(j) > threshold) then final.(wf).(hf) <- 255;
      done;
    done;
    final;;

let symh image =
  
  let w, h = dim image in
  let final = Array.make_matrix w h 0 in

  for (i = h-1) downto 0 do
    for (j = w-1) downto 0 do
      final.((h-1)-i).((w-1)-j) <- image.(i).(j)
    done;
  done;
  final;

  let symv image =

    let w, h = dim image in
    let final = Array.make_matrix w h 0 in

    for (i = 0) to (h - 1) do
      for (j = w-1) downto 0 do
        final.(i).((w-1)-j) <- image.(i).(j)
      done;
    done;
    final;;

let reduction_moitie_ligne array =

    let length = ((Array.length array)/2) in
    let final = Array.make length 0 in
    
    let i = ref 0 in
    
    while (!i <= length) do 
      final.((!i)/2) <- ((array.(!i) + array.(!i + 1))/2);
      i := !i + 2;
    done;
    final;;

let reduction_moitie_image image =

  let w, h = dim image in

  for (i = 0) to (h - 1) do
    image.(i) <- reduction_moitie_ligne image.(i)
  done;
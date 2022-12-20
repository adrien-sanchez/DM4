open Images

let () = Graphics.open_graph "";;

let img = Png. load "flamantsNB.png" [];;
let g = Graphic_image.of_image img;;

Graphics.draw_image g 0 0;;

Unix.sleep 5;;
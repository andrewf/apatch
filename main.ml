open Segments;;
open Util;;


let println s = (print_string s; print_string "\n");;

let maybestr m =
    match m with
    | Some seg -> str_of_patchSegment seg
    | None -> "None" ;;


println (str_of_patchSegment (Reader (KeepChars 34)));;
println (str_of_patchSegment (Reader (DelChars 34)));;
println (maybestr (advance (InsChars "the lazy dog jumped whatever") 5));;
println (maybestr (advance (Reader (KeepChars 34)) 14));;
println (maybestr (advance (Reader (DelChars 34)) 14));;
println (maybestr (advance (Reader (DelChars 14)) 14));;


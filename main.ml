open Printf;;
open Segments;;
open Util;;


let println s = (print_string s; print_string "\n");;

let maybestr m =
    match m with
    | Some seg -> str_of_patchSegment seg
    | None -> "None" ;;


println (str_of_patchSegment (KeepChars 34));;
println (str_of_patchSegment (DelChars 34));;
printf "4 %d; 0 %d\n" (readDim (DelChars 4)) (writeDim (DelChars 4));;
printf "4 %d; 4 %d\n" (readDim (KeepChars 4)) (writeDim (KeepChars 4));;
printf "0 %d; 4 %d\n" (readDim (InsChars "abcd")) (writeDim (InsChars "abcd"));;


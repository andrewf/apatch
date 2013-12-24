open Printf;;
open Segments;;
open Util;;


let println s = (print_string s; print_string "\n");;

println (str_of_patchSegment (KeepChars 34));;
println (str_of_patchSegment (DelChars 34));;
printf "4 %d; 0 %d\n" (readDim (DelChars 4)) (writeDim (DelChars 4));;
printf "4 %d; 4 %d\n" (readDim (KeepChars 4)) (writeDim (KeepChars 4));;
printf "0 %d; 4 %d\n" (readDim (InsChars "abcd")) (writeDim (InsChars "abcd"));;

let testdata = [
    ([], [], []);
    ([ins "foo"], [del 4], [del 4; ins "foof"]);
    ([keep 4], [ins "ex"; del 2], [ins "ex"; del 2])
];;

let test_tuple t =
    let (a, b, c) = t in
        if not ((apply a b) = c) then
            (*printf "Test %s * %s failed" (str_of_patch a) (str_of_patch b)*)
            printf "Test failed.\n"
;;

List.iter test_tuple testdata;;



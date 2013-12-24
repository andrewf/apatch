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
    ([ins "foo"], [del 4], [del 4; ins "foo"]);
    ([keep 4], [ins "ex"; del 2; keep 2], [ins "ex"; del 2; keep 2])
];;

let test_tuple t =
    let (a, b, c) = t in try
        if not ((apply a b) = c) then
            printf "Test %s * %s failed.\n" (str_of_patch a) (str_of_patch b)
    with Failure s -> (printf "Test %s * %s blew up: %s.\n" (str_of_patch a) (str_of_patch b) s)
;;

List.iter test_tuple testdata;;


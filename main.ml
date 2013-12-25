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
    ([], [del 4], [del 4]);
    ([ins "feuyd"], [], [ins "feuyd"]);
    ([ins "foo"], [del 4], [del 4; ins "foo"]);
    (* lhs del *)
    ([del 5], [keep 5], [del 5]);
    ([del 5], [keep 3; ins "af"], [del 3]);
    ([del 5;keep 2], [keep 7], [del 5;keep 2]);
    ([del 5], [ins "abcde"], []);
    ([del 5; keep 2], [ins "abcdefg"], [ins "fg"]);
    ([del 5], [ins "abc"; keep 2], [del 2]);
    (* lhs keep *)
    ([keep 5], [keep 5], [keep 5]);
    ([keep 5], [keep 3; ins "ab"], [keep 3; ins "ab"]);
    ([keep 5; del 2], [keep 7], [keep 5; del 2]);
    ([keep 5], [ins "wxyza"], [ins "wxyza"]);
    ([keep 5], [ins "efg"; keep 2], [ins "efg"; keep 2]);
    ([keep 5; del 2], [ins "abcdefg"], [ins "abcde"]);
    (* no repeated segments! *)
    ([ins "abc"; keep 3; ins "def"], [ins "xyz"], [ins "abcxyzdef"]);
    ([del 4], [del 3; keep 4; del 7], [del 3; del 4; del 7]);
    (* misc *)
    ([keep 4], [ins "ex"; del 2; keep 2], [ins "ex"; del 2; keep 2])
];;

let test_tuple t =
    let (a, b, c) = t in try
        let result = (apply a b) in
        if not (result = c) then
            (printf "Test \"%s\" * \"%s\" failed. \"%s\" != \"%s\".\n"
                (str_of_patch a)
                (str_of_patch b)
                (str_of_patch result)
                (str_of_patch c))
    with Failure s -> (printf "Test %s * %s blew up: %s.\n" (str_of_patch a) (str_of_patch b) s)
;;

List.iter test_tuple testdata;;


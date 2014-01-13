open Printf;;
open Patch;;

module Tf = Textformat;;

let println = print_endline;;

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
    ([del 4], [del 3; keep 4; del 7], [del 14]);
    (* misc *)
    ([keep 4], [ins "ex"; del 2; keep 2], [ins "ex"; del 2; keep 2])
];;

let test_tuple t =
    let (a, b, c) = t in try
        let result = (apply a b) in
        if not (result = c) then
            (printf "Test %s * %s failed. %s != %s.\n"
                (str_of_patch a)
                (str_of_patch b)
                (str_of_patch result)
                (str_of_patch c))
    with Failure s -> (printf "Test %s * %s blew up: %s.\n" (str_of_patch a) (str_of_patch b) s)
;;

println "testing application";;
List.iter test_tuple testdata;;
printf "%d cases tested.\n" (List.length testdata);;

let texttest = [
    (* pretend / is backslash in these comments, because OCaml is annoying *)
    ("\\", [ins "\\"]);     (* "/" -> ins "/" *)
    ("\\z", [ins "\\z"]);   (* "/z" -> ins "/z" *)
    ("\\\\", [ins "\\\\"]); (* "//" -> ins "//" *)
    ("\\%", [ins "\\%"]);   (* "/%" -> ins "/%" *)
    ("\\%[", [ins "%["]);   (* "/%[" -> ins "%[" *)
    ("\\%\\\\", [ins "\\%\\\\"]); (* "/%//" -> "/%//" *)
    ("%[D23]", [del 23]);
    ("%[K34]", [keep 34]);
    ("freep %[K5]%[D3]zuu%[D4]", [ins "freep "; keep 5; del 3; ins "zuu"; del 4]);
    ("abcd%[K3]\\%[xx%[D2]f", [ins "abcd"; keep 3; ins "%[xx"; del 2; ins "f"])
];;

println "testing reading text format";;
List.iter (fun t -> let (data, expected) = t in try
               let result = (Textformat.readString data) in begin
                   printf "Testing \"%s\"\n" data;
                   if not (result = expected) then
                       (printf "  Test \"%s\" failed. %s != %s.\n"
                                data (str_of_patch result) (str_of_patch expected));
                   let reserialized = (Tf.writeString result) in
                       if not (reserialized = data) then
                           (printf "  Re-serialized doesn't match. \"%s\" != \"%s\".\n"
                                    reserialized data);
                end
           with Failure s -> (printf "Test \"%s\" blew up: %s.\n" data s))
           texttest;;
printf "%d cases tested.\n" (List.length texttest);;

let commtest = [
    (* (patch, base), (base', patch') *)
    (* simple, non-overlapping cases *)
    ( ([], [del 4]), ([del 4], [keep 4]) );
    ( ([ins "cat"], []), ([keep 3], [ins "cat"]) );
    ( ([keep 3], [keep 3]), ([keep 3], [keep 3]) );
    ( ([keep 3], [ins "gob"]), ([ins "gob"], []) );
    ( ([del 3], [ins "abc"]), ([], []) );
    ( ([del 3], [keep 3]), ([], [del 3]) );
    (* original cases *)
    ( ([keep 7; ins "mno"; keep 3], [keep 2; ins "xyz"; keep 5]), ([keep 2; ins "xyz"; keep 8], [keep 4; ins "mno"; keep 3]) );
    ( ([keep 2; ins "AB"; keep 2], [keep 1; ins "xy"; keep 1]), ([keep 1; ins "x"; keep 2; ins "y"; keep 1], [keep 1; ins "AB"; keep 1]) );
    (* OT/patch-theory-ish test cases *)
    ( ([keep 2; del 1; ins "b"], [keep 2; del 2; ins "t"]), ([keep 2; del 2; keep 1], [keep 4; ins "b"]) )
];;

println "testing commutation";;
List.iter (fun ((patch, base), (expected_comm_base, expected_comm_patch)) -> try
               let (comm_base, comm_patch) = commute patch base in
                   if not ((comm_base = expected_comm_base) && (comm_patch = expected_comm_patch)) then
                       printf "Test %s * %s failed:\n  comm_base: %s, expected %s\n  comm_patch: %s, expected %s\n"
                              (str_of_patch patch)
                              (str_of_patch base)
                              (str_of_patch comm_base)
                              (str_of_patch expected_comm_base)
                              (str_of_patch comm_patch)
                              (str_of_patch expected_comm_patch)
           with Failure s -> (printf "Test %s * %s blew up: %s.\n" (str_of_patch patch) (str_of_patch base) s))
           commtest;;

printf "%d cases tested.\n" (List.length commtest);;



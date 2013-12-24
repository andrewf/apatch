type readerSegment = KeepChars of int
                   | DelChars of int
;;

type patchSegment = Reader of readerSegment
                  | InsChars of string
;;

let str_of_readerSegment r =
    match r with
    | KeepChars n -> "{K" ^ string_of_int n ^ "}"
    | DelChars n  -> "{D" ^ string_of_int n ^ "}"

let str_of_patchSegment segment =
    match segment with
    | Reader r -> str_of_readerSegment r
    | InsChars s -> s ;;

let strdrop s n = String.sub s n ((String.length s) - n);;

let advanceReader r dist=
    match r with
        | KeepChars n ->
            if dist < n then
                KeepChars (n - dist)
            else
                failwith "advanced KeepChars too far"
        | DelChars n ->
            if dist < n then
                DelChars (n - dist)
            else
                failwith "advanced DelChars too far"
;;

let advance segment dist = 
    match segment with
    | Reader r -> Reader (advanceReader r dist)
    | InsChars s ->
        try
            InsChars (strdrop s dist)
        with Invalid_argument msg -> failwith "advanced InsChars too far"
;;

let println s = (print_string s; print_string "\n");;

println (str_of_patchSegment (Reader (KeepChars 34)));;
println (str_of_patchSegment (Reader (DelChars 34)));;
println (str_of_patchSegment (advance (InsChars "the lazy dog jumped whatever") 5));;
println (str_of_patchSegment (advance (Reader (KeepChars 34)) 14));;
println (str_of_patchSegment (advance (Reader (DelChars 34)) 14));;


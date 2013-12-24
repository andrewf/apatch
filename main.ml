type patchSegment = KeepChars of int
                  | DelChars of int
                  | InsChars of string

let str_of_patchSegment segment =
    match segment with
     | KeepChars n ->
        "{K" ^ string_of_int n ^ "}"
     | DelChars n ->
        "{D" ^ string_of_int n ^ "}"
     | InsChars s ->
        s ;;

let strdrop s n = String.sub s n ((String.length s) - n);;

let advance segment dist = 
    match segment with
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
        | InsChars s ->
            try
                InsChars (strdrop s dist)
            with Invalid_argument msg -> failwith "advanced InsChars too far"
;;

print_string ((str_of_patchSegment (advance (InsChars "the lazy dog jumped whatever") 5)) ^ "\n");;


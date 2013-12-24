open Segments;;

let str_of_readerSegment r =
    match r with
    | KeepChars n -> "{K" ^ string_of_int n ^ "}"
    | DelChars n  -> "{D" ^ string_of_int n ^ "}"

let str_of_patchSegment segment =
    match segment with
    | Reader r -> str_of_readerSegment r
    | InsChars s -> s ;;


let strdrop s n = String.sub s n ((String.length s) - n);;

(* Reader -> option Reader *)
let advanceReader r dist =
    match r with
        | KeepChars n ->
            if dist < n then
                Some (KeepChars (n - dist))
            else if (dist = n) then
                None
            else
                failwith "advanced KeepChars too far"
        | DelChars n ->
            if dist < n then
                Some (DelChars (n - dist))
            else if (dist = n) then
                None
            else
                failwith "advanced DelChars too far"
;;

let advance segment dist = 
    match segment with
    | Reader r ->
		(let maybe = (advanceReader r dist) in
			match maybe with
			| Some r -> Some (Reader r)
			| None -> None)
    | InsChars s ->
        try
            if (String.length s) = dist then
                None
            else
                Some (InsChars (strdrop s dist))
        with Invalid_argument msg -> failwith "advanced InsChars too far"
;;



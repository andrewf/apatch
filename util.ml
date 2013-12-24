open Segments;;

let str_of_patchSegment segment =
    match segment with
    | KeepChars n -> "{K" ^ string_of_int n ^ "}"
    | DelChars n  -> "{D" ^ string_of_int n ^ "}"
    | InsChars s -> s ;;

let str_of_patch p =
    match p with
    | px::pxs ->
    "[" ^ (List.fold_left
                (fun prev seg -> (prev ^ ", " ^ (str_of_patchSegment seg)))
                (str_of_patchSegment px)
                pxs) ^ "]"
    | [] -> "[]"
;;
    


let strdrop s n = String.sub s n ((String.length s) - n);;
let strtake s n = String.sub s 0 n;;

(* how many chars this segment will read *)
let readDim seg =
    match seg with
    | KeepChars n -> n
    | DelChars n -> n
    | InsChars _ -> 0
;;

let writeDim seg =
    match seg with
    | KeepChars n -> n
    | DelChars n -> 0
    | InsChars s -> String.length s
;;


let rec apply lhs rhs =
    match(lhs, rhs) with
    | ([], []) -> []
    (* copy up rhs del *)
    | (_, (DelChars n)::rxs) -> (DelChars n) :: (apply lhs rxs)
    (* copy down lhs ins *)
    | ( (InsChars s)::lxs, _) -> (InsChars s) :: (apply lxs rhs)

    (* lhs del *)
    | ( (DelChars m)::lxs, (KeepChars n)::rxs) ->
        let consumed = (min m n) in
        (DelChars consumed) :: (apply 
            (if m > consumed then (del (m - consumed))::lxs else lxs)
            (if n > consumed then (keep (n - consumed))::rxs else rxs)
        )

    | ( (DelChars m)::lxs, (InsChars s)::rxs) ->
        let slen = (String.length s) in
        let consumed = (min m slen) in
        (apply
            (if m > consumed then (del (m-consumed))::lxs else lxs)
            (if slen > consumed then (ins (strdrop s consumed))::rxs else rxs)
        )

    (* lhs keep *)
    | ( (KeepChars m)::lxs, (KeepChars n)::rxs) ->
        let consumed = (min m n) in
        (KeepChars (min m n)) :: (apply
            (if m > consumed then (keep (m - consumed))::lxs else lxs)
            (if n > consumed then (keep (n - consumed))::rxs else rxs)
        )

    | ( (KeepChars m)::lxs, (InsChars s)::rxs) ->
        let slen = (String.length s) in
        let consumed = (min m slen) in
        (InsChars (strtake s consumed)) :: (apply
            (if m > consumed then (keep (m-consumed))::lxs else lxs)
            (if slen > consumed then (InsChars (strdrop s consumed))::rxs else rxs)
        )

    (* starved reader error *)
    | ( (KeepChars _)::_, []) -> failwith "Starved keeper"
    | ( (DelChars _)::_, []) -> failwith "Starved deleter"
    (* dangling writer error *)
    | ( [], (InsChars _)::_ ) -> failwith "Dangling insert"
    | ( [], (KeepChars _)::_) -> failwith "Dangling Keeper in source"
;;


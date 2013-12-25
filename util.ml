open Segments;;
open Str;; (* for first/last_chars *)

let str_of_patchSegment segment =
    match segment with
    | KeepChars n -> "{K" ^ string_of_int n ^ "}"
    | DelChars n  -> "{D" ^ string_of_int n ^ "}"
    | InsChars s -> s ;;

let str_of_patch p =
    match p with
    | px::pxs ->
        (List.fold_left
                (fun prev seg -> (prev ^ (str_of_patchSegment seg)))
                (str_of_patchSegment px)
                pxs)
    | [] -> ""
;;
    
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

let advance patch consumed =
    match patch with
    | [] -> failwith "tried to advance empty patch"
    | (KeepChars n)::pxs -> begin
        if n > consumed then
            (keep (n-consumed))::pxs
        else if n = consumed then
            pxs
        else failwith "advanced keep segment too far"
    end
    | (DelChars n)::pxs -> begin
        if n > consumed then
            (del (n-consumed))::pxs
        else if n = consumed then
            pxs
        else failwith "advanced del segment too far"
    end
    | (InsChars s)::pxs ->
        let slen = String.length s in begin
            if slen > consumed then
                (ins (last_chars s (slen-consumed)))::pxs
            else if slen = consumed then
                pxs
            else failwith "advanced ins segment too far"
        end
;;

(* cons, but check for runs of same-typed segments first *)
let defrag newhead applied =
    match (newhead, applied) with
    | (InsChars s, (InsChars t)::rest) ->
        (InsChars (s^t))::rest     (* this could become a performance problem *)
    | (DelChars n, (DelChars m)::rest) ->
        (DelChars (n+m))::rest
    | (_, _) ->
        newhead::applied
;;

let rec apply lhs rhs =
    match(lhs, rhs) with
    (* base case *)
    | ([], []) -> []

    (* copy up rhs del *)
    | (_, (DelChars n)::rxs) ->
        defrag (DelChars n) (apply lhs rxs)

    (* copy down lhs ins *)
    | ( (InsChars s)::lxs, _) ->
        defrag (InsChars s) (apply lxs rhs)

    (* lhs del *)
    | ( (DelChars m)::lxs, (KeepChars n)::rxs) ->
        let consumed = (min m n) in
        defrag (DelChars consumed) (apply (advance lhs consumed) (advance rhs consumed))

    | ( (DelChars m)::lxs, (InsChars s)::rxs) ->
        let slen = (String.length s) in
        let consumed = (min m slen) in
        apply (advance lhs consumed) (advance rhs consumed)
        
    (* lhs keep *)
    | ( (KeepChars m)::lxs, (KeepChars n)::rxs) ->
        let consumed = (min m n) in
        (KeepChars (min m n)) :: (apply (advance lhs consumed) (advance rhs consumed))

    | ( (KeepChars m)::lxs, (InsChars s)::rxs) ->
        let slen = (String.length s) in
        let consumed = (min m slen) in
        defrag (InsChars (Str.first_chars s consumed)) (apply (advance lhs consumed) (advance rhs consumed))

    (* starved reader error *)
    | ( (KeepChars _)::_, []) -> failwith "Starved keeper"
    | ( (DelChars _)::_, []) -> failwith "Starved deleter"
    (* dangling writer error *)
    | ( [], (InsChars _)::_ ) -> failwith "Dangling insert"
    | ( [], (KeepChars _)::_) -> failwith "Dangling Keeper in source"
;;


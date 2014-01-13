open Str;; (* for first/last_chars *)

(* behold, the data structure *)
type segment = KeepChars of int
             | DelChars  of int 
             | InsChars  of string
;;

(* shortcuts *)
let keep n = KeepChars n;;
let del n = DelChars n;;
let ins s = InsChars s;;

(* for debug output *)
let str_of_patchSegment segment =
    match segment with
    | KeepChars n -> "K" ^ string_of_int n 
    | DelChars n  -> "D" ^ string_of_int n
    | InsChars s -> "I\"" ^ s ^ "\"" ;;

let str_of_patch p =
    ("[" ^ (List.fold_left
            (fun prev seg -> (prev ^ (str_of_patchSegment seg) ^ ", "))
            ""
            p) ^ "]")
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
    | (KeepChars n, (KeepChars m)::rest) ->
        (KeepChars (n+m))::rest
    | (_, _) ->
        newhead::applied
;;

(* behold, the function. lhs is the patch, rhs the source *)
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
        defrag (DelChars consumed)
               (apply (advance lhs consumed) (advance rhs consumed))

    | ( (DelChars m)::lxs, (InsChars s)::rxs) ->
        let slen = (String.length s) in
        let consumed = (min m slen) in
        apply (advance lhs consumed) (advance rhs consumed)
        
    (* lhs keep *)
    | ( (KeepChars m)::lxs, (KeepChars n)::rxs) ->
        let consumed = (min m n) in
        (* this defrag only matters for patches that start off with repeated segments *)
        defrag (KeepChars (min m n))   
               (apply (advance lhs consumed) (advance rhs consumed))

    | ( (KeepChars m)::lxs, (InsChars s)::rxs) ->
        let slen = (String.length s) in
        let consumed = (min m slen) in
        defrag (InsChars (Str.first_chars s consumed))
               (apply (advance lhs consumed) (advance rhs consumed))

    (* starved reader error *)
    | ( (KeepChars _)::_, []) -> failwith "Starved keeper"
    | ( (DelChars _)::_, []) -> failwith "Starved deleter"
    (* dangling writer error *)
    | ( [], (InsChars _)::_ ) -> failwith "Dangling insert in source"
    | ( [], (KeepChars _)::_) -> failwith "Dangling Keeper in source"
;;

(* patch -> base -> (commuted base, commuted patch) *)
let rec commute patch base =
    let maybedefrag maybeseg tail = match maybeseg with None -> tail | Some seg -> defrag seg tail in
    let yield (new_comm_base_seg, new_patch_base_seg) (patch, base) = begin
        let (comm_base_tail, comm_patch_tail) = commute patch base in
        ( maybedefrag new_comm_base_seg comm_base_tail, maybedefrag new_patch_base_seg comm_patch_tail )
    end in
    match (patch, base) with
    | ([], []) -> ([], [])
    (* rhs del *)
    | (_, (DelChars n)::bxs) ->
        yield (Some (DelChars n), Some (KeepChars n)) (patch, advance base n)
    
    (* lhs ins *)
    | ((InsChars s)::pxs, _) ->
        let slen = String.length s in
        yield (Some (KeepChars slen), Some (InsChars s)) (advance patch slen, base)

    (* K*K *)
    | ((KeepChars n)::pxs, (KeepChars m)::bxs) ->
        let consumed = min n m in
        yield (Some (KeepChars consumed), Some(KeepChars consumed)) (advance patch consumed, advance base consumed)

    (* K*I *)
    | ((KeepChars n)::pxs, (InsChars s)::bxs) ->
        let consumed = min n (String.length s) in
        yield (Some (InsChars (Str.first_chars s consumed)), None) (advance patch consumed, advance base consumed)

    (* D * K *)
    | ((DelChars n)::pxs, (KeepChars m)::bxs) ->
        let consumed = min n m in
        yield (None, Some (DelChars consumed)) (advance patch consumed, advance base consumed)

    (* D*I *)
    | ((DelChars n)::_, (InsChars s)::_) ->
        let consumed = min n (String.length s) in
        yield (None, None) (advance patch consumed, advance base consumed)

    (* starved reader error *)
    | ( (KeepChars _)::_, []) -> failwith "Starved keeper"
    | ( (DelChars _)::_, []) -> failwith "Starved deleter"
    (* dangling writer error *)
    | ( [], (InsChars _)::_ ) -> failwith "Dangling insert in source"
    | ( [], (KeepChars _)::_) -> failwith "Dangling Keeper in source"
;;

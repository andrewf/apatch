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
    if consumed > 0 then
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
    else
        patch
;;

(* cons, but check for runs of same-typed segments first *)
let defrag newhead applied =
    match newhead, applied with
    | (InsChars s, (InsChars t)::rest) ->
        (InsChars (s^t))::rest     (* this could become a performance problem *)
    | (DelChars n, (DelChars m)::rest) ->
        (DelChars (n+m))::rest
    | (KeepChars n, (KeepChars m)::rest) ->
        (KeepChars (n+m))::rest
    | (_, _) ->
        newhead::applied
;;

(* defrag, but make sure del is in front of ins where possible. Basically, put
   InsChars behind DelChars *)
let normal_defrag newhead applied =
    match newhead, applied with
    | (InsChars s), (DelChars n)::(InsChars z)::tail ->
        (DelChars n)::(InsChars (s ^ z))::tail
    | (InsChars s), (DelChars n)::tail ->
        (DelChars n)::(InsChars s)::tail
    | _ ->
        defrag newhead applied
;;

let maybe_defrag seg tail =
    match seg with
    | None -> tail
    | Some s -> normal_defrag s tail
;;

(* returns a function you can use as yield (segment option) patchAction patchAction *)
let yield_with patch base make_rest =
    fun seg patch_consumed base_consumed -> begin
        let newTail = make_rest (advance patch patch_consumed) (advance base base_consumed) in
        maybe_defrag seg newTail
    end
;;

(* behold, the function. patch is the patch, base the source *)
let rec apply patch base =
    let yield = yield_with patch base apply in
    match patch, base with
    (* base case *)
    | ([], []) -> []

    (* copy up base del *)
    | _, (DelChars n)::rxs ->
        yield (Some (DelChars n)) 0 n

    (* copy down patch ins *)
    | (InsChars s)::lxs, _ ->
        yield (Some (InsChars s)) (String.length s) 0

    (* patch del *)
    | (DelChars m)::lxs, (KeepChars n)::rxs ->
        let consumed = (min m n) in
        yield (Some (DelChars consumed)) consumed consumed

    | (DelChars m)::lxs, (InsChars s)::rxs ->
        let slen = (String.length s) in
        let consumed = (min m slen) in
        yield None consumed consumed
        
    (* patch keep *)
    | (KeepChars m)::lxs, (KeepChars n)::rxs ->
        let consumed = (min m n) in
        (* this defrag only matters for patches that start off with repeated segments *)
        yield (Some (KeepChars consumed)) consumed consumed

    | (KeepChars m)::lxs, (InsChars s)::rxs ->
        let slen = (String.length s) in
        let consumed = (min m slen) in
        yield (Some (InsChars (Str.first_chars s consumed))) consumed consumed

    (* starved reader error *)
    | (KeepChars _)::_, [] -> failwith "Starved keeper"
    | (DelChars _)::_, [] -> failwith "Starved deleter"
    (* dangling writer error *)
    | [], (InsChars _)::_  -> failwith "Dangling insert in source"
    | [], (KeepChars _)::_ -> failwith "Dangling Keeper in source"
;;

(* patch -> base -> (commuted base, commuted patch) *)
let rec commute patch base =
    let yield (new_comm_base_seg, new_patch_base_seg) (patch_consumed, base_consumed) = begin
        let (comm_base_tail, comm_patch_tail) = commute (advance patch patch_consumed) (advance base base_consumed) in
        ( maybe_defrag new_comm_base_seg comm_base_tail, maybe_defrag new_patch_base_seg comm_patch_tail )
    end in
    match patch, base with
    | [], [] -> [], []
    (* rhs del *)
    | _, (DelChars n)::bxs ->
        yield (Some (DelChars n), Some (KeepChars n))
              (0, n)
    
    (* lhs ins *)
    | (InsChars s)::pxs, _ ->
        let slen = String.length s in
        yield (Some (KeepChars slen), Some (InsChars s))
              (slen, 0)

    (* K*K *)
    | (KeepChars n)::pxs, (KeepChars m)::bxs ->
        let consumed = min n m in
        yield (Some (KeepChars consumed), Some(KeepChars consumed))
              (consumed, consumed)

    (* K*I *)
    | (KeepChars n)::pxs, (InsChars s)::bxs ->
        let consumed = min n (String.length s) in
        yield (Some (InsChars (Str.first_chars s consumed)), None)
              (consumed, consumed)

    (* D * K *)
    | (DelChars n)::pxs, (KeepChars m)::bxs ->
        let consumed = min n m in
        yield (None, Some (DelChars consumed))
              (consumed, consumed)

    (* D*I *)
    | (DelChars n)::_, (InsChars s)::_ ->
        let consumed = min n (String.length s) in
        yield (None, None)
              (consumed, consumed)

    (* starved reader error *)
    | (KeepChars _)::_, [] -> failwith "Starved keeper"
    | (DelChars _)::_, [] -> failwith "Starved deleter"
    (* dangling writer error *)
    | [], (InsChars _)::_  -> failwith "Dangling insert in source"
    | [], (KeepChars _)::_ -> failwith "Dangling Keeper in source"
;;


(* invert patch with respect to base. That is, the inverse of D will
   insert what was in the base there, if it was chars. This will fail for D*K columns,
   because we don't (and can't!) know how to invert those *)
let rec invert patch base =
    let yield = yield_with patch base invert in
    match patch, base with
    | [], [] -> []
    (* base del is wholly irrelevant *)
    | _, (DelChars n)::_ ->
        yield None 0 n
    | (InsChars s)::_, _ ->
        let len = String.length s in
        yield (Some (DelChars len)) len 0
    | (DelChars n)::_, (KeepChars m)::_ ->
        failwith "can't invert D*K"
    | (DelChars n)::_, (InsChars s)::_ ->
        let len = String.length s in
        let m = min n len in
        yield (Some (InsChars (Str.first_chars s m))) m m
    | (KeepChars n)::_, _::_ -> (* base must have at least one element for keep to be valid *)
        yield (Some (KeepChars n)) n n
    (* starvation/dangling errors *)
    | (KeepChars _)::_, [] -> failwith "Starved keeper while inverting"
    | (DelChars _)::_, [] -> failwith "Starved deleter while inverting"
    | [], (InsChars _)::_  -> failwith "Dangling insert in base while inverting"
    | [], (KeepChars _)::_ -> failwith "Dangling Keeper in base while inverting"
;;

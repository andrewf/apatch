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
let strtake s n = String.sub s 0 n;;

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

(* how many chars this segment will read *)
let readDim seg =
    match seg with
    | Reader (KeepChars n) -> n
    | Reader (DelChars n) -> n
    | InsChars _ -> 0
;;

let writeDim seg =
    match seg with
    | Reader (KeepChars n) -> n
    | Reader (DelChars n) -> 0
    | InsChars s -> String.length s
;;


(*(* apply a reading lhs to writing rhs. Returns the appropriate
resulting segment (option), and the number of chars in both sides consumed *)
let applyCompatible lhs, rhs =
    let consumed = min (readDim lhs) (writeDim rhs) in
        match (lhs, rhs) with
        | (Reader (DelChars l))

let rec apply lhs rhs =
    let lx::lxs = lhs and rx::rxs = rhs in
        match (lx, rx) with
        (* copy up D from rhs *)
        | (_, Reader (DelChars n)) -> []
        (* Insert chars from lhs *)
        | (InsChars s, _) -> lx :: (apply lxs rhs)
        | (Reader lr, _) -> begin
            match(lr, rx) with
            | (DelChars m, Reader (KeepChars n)) -> []
            | (DelChars m, InsChars s) -> []
            | (KeepChars m, Reader (KeepChars n)) -> []
            | (KeepChars m, InsChars s) -> []
        end
;;
*)
let rec apply lhs rhs =
    match(lhs, rhs) with
    | ([], []) -> []
    (* copy up rhs del *)
    | (_, (Reader (DelChars n)) :: rxs) -> (Reader (DelChars n)) :: (apply lhs rxs)
    (* copy down lhs ins *)
    | ( (InsChars s)::lxs, _) -> (InsChars s) :: (apply lxs rhs)

    (* lhs del *)
    | ( (Reader (DelChars m))::lxs, (Reader (KeepChars n))::rxs) ->
        let consumed = (min m n) in
        (Reader (DelChars consumed)) :: (apply 
            (if m > consumed then (del (m - consumed))::lxs else lxs)
            (if n > consumed then (keep (n - consumed))::rxs else rxs)
        )

    | ( (Reader (DelChars m))::lxs, (InsChars s)::rxs) ->
        let slen = (String.length s) in
        let consumed = (min m slen) in
        (apply
            (if m > consumed then (del (m-consumed))::lxs else lxs) (* carry over leftover deletion *)
            (if slen > consumed then (ins (strdrop s consumed))::rxs else rxs) (* ditto leftover text *)
        )

    (* lhs keep *)
    | ( (Reader (KeepChars m))::lxs, (Reader (KeepChars n))::rxs) ->
        let consumed = (min m n) in
        (Reader (KeepChars (min m n))) :: (apply
            (if m > consumed then (keep (m - consumed))::lxs else lxs)
            (if n > consumed then (keep (n - consumed))::rxs else rxs)
        )

    | ( (Reader (KeepChars m))::lxs, (InsChars s)::rxs) ->
        let slen = (String.length s) in
        let consumed = (min m slen) in
        (InsChars (strtake s consumed)) :: (apply
            (if m > consumed then (keep (m-consumed))::lxs else lxs)
            (if slen > consumed then (InsChars (strdrop s consumed))::rxs else rxs)
        )

    (* starved reader error *)
    | ( (Reader _)::_, []) -> failwith "Starved reader"
    (* dangling writer error *)
    | ( [], (InsChars _)::_ ) -> failwith "Dangling insert"
    | ( [], (Reader (KeepChars _))::_) -> failwith "Dangling Keeper in source"
;;


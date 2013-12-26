open Str;;

let escapeChar = '\\';;
let sigil = '%';;
let opener = '[';;
let closer = ']';;
let keepChar = 'K';;
let delChar  = 'D';;

type readerState = Inserting of string (* start state is `Inserting ""` *)
                 (* these two are for handling escape sequences *)
                 | EscapeFound of string
                 | EscapedSigilFound of string
                 (* for parsing reader forms *)
                 | ExpectingOpener of string (* still need to keep insert buffer in
                                                case we don't find opener *)
                 | ExpectingType (* next char is D or K *)
                 | ReadingN of string * (int -> Patch.segment);;

let string_of_char c = String.make 1 c;;
let digit_regex = regexp "[0123456789]$";;
let is_digit c = string_match digit_regex (string_of_char c) 0;;

(* state -> c -> (new state, new segment option *)
let advanceReader state c =
    let yield s = (Inserting "", Some s) in
    let transition st = (st, None) in
    match state with
    | Inserting s -> begin
        match c with
        | z when z=escapeChar -> transition (EscapeFound s)
        | z when z=sigil -> transition (ExpectingOpener s)
        | _ -> transition (Inserting (s ^ (string_of_char c)))
    end
    | EscapeFound s -> begin
        (* backslash found, expecting sigil, maybe *)
        match c with
        | z when z=sigil -> transition (EscapedSigilFound s)
        (* false alarm, dump everything matched so far to the insert seg *)
        | _ -> transition (Inserting (s ^ (string_of_char escapeChar)))
    end
    | EscapedSigilFound s -> begin
        match c with
        (* found whole escape sequence, put escaped chars in insert stream *)
        | z when z=opener ->
            transition (Inserting (s ^ (string_of_char sigil) ^ (string_of_char opener)))
        | _ ->
            transition (Inserting (s ^ (string_of_char escapeChar)
                                ^ (string_of_char sigil)
                                ^ (string_of_char opener)))
    end
    | ExpectingOpener s -> begin
        (* sigil found, maybe we'll see opening bracket *)
        match c with
        | z when z=opener -> (ExpectingType, if not (s="") then Some (Patch.InsChars s) else None)
        (* false alarm, dump matched char *)
        | _ -> transition (Inserting (s ^ (string_of_char sigil)))
    end
    | ExpectingType -> begin
        match c with
        | z when z=keepChar ->
            transition (ReadingN ("", (fun n -> Patch.KeepChars n)))
        | z when z=delChar ->
            transition (ReadingN ("", (fun n -> Patch.DelChars n)))
        | _ -> failwith "oh noes, invalid reader form"
    end
    | ReadingN (s, ctor) -> begin
        match c with
        | digit when is_digit(digit) ->
            transition (ReadingN ((s^(string_of_char digit)), ctor))
        | z when z=closer ->
            yield (ctor (int_of_string s))
        | _ ->
            failwith "non-digit char in reader form"
    end
;;

let finishReading state =
    match state with
    | Inserting "" -> None
    | Inserting s -> Some (Patch.InsChars s)
    | EscapeFound s -> Some (Patch.InsChars (s ^ (string_of_char escapeChar)))
    | EscapedSigilFound s -> Some (Patch.InsChars (s ^ (string_of_char escapeChar)
                                                     ^ (string_of_char sigil)))
    | ExpectingOpener s -> Some (Patch.InsChars (s ^ (string_of_char sigil)))
    | _ -> failwith "File ended inside read form"
;;

let rec reverse l =
    let rec helper rest list_before =
        match rest with
        | [] -> []
        | [x] -> x :: list_before
        | x :: xs -> (helper xs (x::list_before))
    in helper l []
;;

let readString str =
    let state = ref (Inserting "") in
    let result = ref [] in
    (* This builds list in reverse order! *)
    let maybeAdd seg = (match seg with Some s -> (result := s::(!result)) | None -> () ) in begin
        String.iter (fun c -> 
            let newstate, yielded = advanceReader !state c in begin
                state := newstate;
                maybeAdd yielded
            end) str;
        maybeAdd (finishReading !state);
        reverse !result
    end
;;



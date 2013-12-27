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

class patchBuilder = object(self)
    val mutable state = Inserting ""
    val mutable result = ([] : Patch.segment list)
    method maybeAddSeg seg =
        match seg with
        | Some s -> (result <- s::result)
        | None -> ()
    method addChar c =
        (* this algo builds result in reverse order *)
        let newstate, yielded = advanceReader state c in begin
            state <- newstate;
            self#maybeAddSeg yielded
        end
    method finish =
        self#maybeAddSeg (finishReading state)
    method getResult = reverse result (* actually, unreverse *)
end

let readString str =
    let builder = new patchBuilder in begin
        String.iter (fun c -> builder#addChar c) str;
        builder#finish;
        builder#getResult
    end
;;

let readFile chan = begin
    let builder = new patchBuilder in (try
        while true do
            builder#addChar (input_char chan)
        done
    with End_of_file -> (builder#finish));
    builder#getResult
end ;;

let escape s =
    let mustEscape = (string_of_char sigil) ^ (string_of_char opener) in
    let r = regexp_string mustEscape in
    global_replace r ((string_of_char escapeChar) ^ mustEscape) s;;

let string_of_segment seg =
    match seg with
    | Patch.InsChars s -> escape s
    | Patch.KeepChars n ->
        ((string_of_char sigil) ^
         (string_of_char opener) ^
         (string_of_char keepChar) ^
         (string_of_int n) ^
         (string_of_char closer))
    | Patch.DelChars n ->
        ((string_of_char sigil) ^
         (string_of_char opener) ^
         (string_of_char delChar) ^
         (string_of_int n) ^
         (string_of_char closer))
;;

let writeString patch =
    (List.fold_left
        (fun prev seg -> (prev ^ (string_of_segment seg)))
        ""
        patch)

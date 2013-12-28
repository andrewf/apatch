(* fuzz test random patches for associativity *)
open Printf;;
open Patch;;

let max_dimension = 2000;;
let iterations = 1000;; (* number of random sets to try *)

let starting_char = int_of_char 'z';; (* keep as int until last second,
                                         so we can do arithmetic *)
let next_char n =
    let min_char = int_of_char 'A' in
    let candidate = n - 1 in
        if candidate < min_char then
            failwith "ran out of test chars"
        else
            candidate
;;

Random.self_init ();;

let rand_upto n =
    Random.int (n+1);;

let rand_1upto n =
    (rand_upto (n-1)) + 1;;

let randchoice3 f1 f2 f3 =
    let flip = rand_upto 1 in
        if flip = 0 then
            f1 ()
        else
            f2 ()
;;

let random_patch start_char write_dim read_dim = (* -> (patch, new start) *)
    let make_ins len c = (InsChars (String.make len (char_of_int c))) in
    let rec helper start_char write_dim read_dim = begin(* -> (patch, new start) *)
        if write_dim = 0 then
            if read_dim = 0 then
                (* no more, return [], same char*)
                [], start_char
            else
                (* only deletes allowed, fill the rest of the space *)
                let thisdim = read_dim in
                    (del thisdim)::[], start_char
        else if read_dim = 0 then
            (* only inserts allowed, fill up the rest of the space *)
            let thisdim = write_dim in
            let nextseg = make_ins thisdim start_char in
                nextseg::[], (next_char start_char)
        else
            (* both >1, anything goes *)
            let thisdim = rand_1upto (min write_dim read_dim) in
            (* generate a random patch segment within the current constraints 
                and get ready values with which to call helper to generate the
                tail of the patch *)
            let nextseg, next_start, write, read =
                randchoice3 (fun () -> make_ins thisdim start_char,
                                       next_char start_char,
                                       (write_dim - thisdim),
                                       read_dim)

                            (fun () -> keep thisdim,
                                       start_char,
                                       (write_dim - thisdim),
                                       (read_dim - thisdim))

                            (fun () -> del thisdim,
                                       start_char,
                                       write_dim,
                                       (read_dim - thisdim))
            in
            let tail, final_start = helper next_start write read in
                nextseg::tail, final_start
    end in
    helper start_char write_dim read_dim
;;

let program_exit = ref 0;;

for i = 1 to iterations do
    (* generate three compatible patches a, b, and c, and check that
       their application is associative *)
    let final_write, middle_write, middle_read, final_read =
        rand_1upto max_dimension, rand_1upto max_dimension, rand_1upto max_dimension,rand_1upto max_dimension in
    let a, ch = random_patch starting_char final_write middle_write in
    let b, ch = random_patch ch middle_write middle_read in
    let c, _  = random_patch ch middle_read final_read in
    let debug_out () = 
        printf "testing:\n  %s *\n  %s *\n  %s\n"
               (str_of_patch a)
               (str_of_patch b)
               (str_of_patch c) in
    try
        let left_assoc = apply a b in
        let right_assoc = apply b c in
        let left_final = apply left_assoc c in
        let right_final = apply a right_assoc in
        if not (left_final = right_final) then begin
            printf "ack, fail";
            debug_out ();
            program_exit := 1;
        end
    with Failure s -> begin
        printf "woops, something caught on fire: %s\n" s;
        debug_out ();
        program_exit := 1
    end
done ;;

if !program_exit == 0 then
    printf "All tests succeeded.\n"
;;

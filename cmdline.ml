(* take a filename on filename to be patch,
   stdin to be source, and write output to stdout *)

open Printf;;

(* we can assume argv[0] exists *)
let usage = sprintf "Usage: %s patchfile" (Array.get Sys.argv 0);;

(* Array.iter (fun arg -> printf "%s\n" arg) Sys.argv;; *)

let eprintf = fprintf stderr;;

if not ((Array.length Sys.argv) = 2) then begin
    eprintf "%s\n" usage;
    exit 1
end

let patchfile = open_in (Array.get Sys.argv 1);;

let patch = Textformat.readFile patchfile;;
let source = Textformat.readFile stdin;;

(* more debug output, geez *)
eprintf "patch (lhs): %s\n" (Patch.str_of_patch patch);;
eprintf "source (rhs): %s\n" (Patch.str_of_patch source);;


try
    print_string (Textformat.writeString (Patch.apply patch source))
with Failure s -> begin
    eprintf "Failed to apply patches: %s\n" s;
    exit 1
end;;

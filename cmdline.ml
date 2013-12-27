(* take a filename on filename to be patch,
   stdin to be source, and write output to stdout *)

open Printf;;

(* we can assume argv[0] exists *)
let usage = sprintf "Usage: %s patchfile" (Array.get Sys.argv 0);;

printf "%d\n" (Array.length Sys.argv);;
Array.iter (fun arg -> printf "%s\n" arg) Sys.argv;;

if not ((Array.length Sys.argv) = 2) then begin
    fprintf stderr "%s\n" usage;
    exit 1
end

let patchfile = open_in (Array.get Sys.argv 1);;

let patch = Textformat.readFile patchfile;;

print_string (Patch.str_of_patch patch);;

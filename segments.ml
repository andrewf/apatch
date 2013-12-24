
type patchSegment = KeepChars of int
                  | DelChars of int 
                  | InsChars of string
;;

let keep n = KeepChars n;;
let del n = DelChars n;;
let ins s = InsChars s;;


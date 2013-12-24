
type readerSegment = KeepChars of int
                   | DelChars of int
;;

type patchSegment = Reader of readerSegment
                  | InsChars of string
;;

let keep n = Reader (KeepChars n);;
let del n = Reader (DelChars n);;
let ins s = InsChars s;;


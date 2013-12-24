
type readerSegment = KeepChars of int
                   | DelChars of int
;;

type patchSegment = Reader of readerSegment
                  | InsChars of string
;;


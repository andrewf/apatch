# apatch

apatch is an implementation of associative text patching in OCaml. My theory
and motivation for associative patching is alread laid out decently at
[adiff](https://github.com/andrewf/adiff), the project for my Clojure
implementation. To understand this project, you can skip the part about
"Nested Patches", since it doesn't matter for text and I'm not sure how
sound it is. The main difference for this project is that I have implemented
the run-length encoding optimization mentioned in the "what about real life?"
section.

## Writing patches

The text format for patches is pretty simple.

    It's a %[D13]beautiful day. %[K34]
    We can do literal \%[K3] too.

The `%[...]` syntax introduces what I call a "reader form", a patch element
that either reads in or ignores/deletes characters from the source. As you
might guess, `%[D13]` drops 13 characters and `%[K34]` copies in 34
characters. `\%[` results in a simple insertion of `%[`, so you can write
patches for documents that talk about patches. The `%[..]` syntax was chosen
for minimal interference with other languages.

## Applying patches

First, compile. If you have OCaml installed on your system, you can simply
`make all`, which builds `apply` and the test programs.

If you want to do patching in an OCaml program, `test.ml` has a number of
examples of creating and applying patches.

Using the command-line `apply` program is pretty simple. In short:
`./apply patch < source > result`. You can try this with the included 
patch files:

    $ ./apply patchy.patch < sourcy.patch
    ABCabcd

Watch out for implicit newlines at the end of files. If you get a mysterious
"dangling insert" error, try increasing the size of a reader form in the
patch by 1.

## Future work

Obviously, these sorts of patches are not much fun to write by hand. Someday
I'll get around to implementing diffing. Also, both "adiff" and "apatch" are
pretty lame names, and I need to invent a better one.

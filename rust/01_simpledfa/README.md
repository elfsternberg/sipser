# SimpleDfa

A simple implementation of the DFA described in chapter one of Michael
Siper's *Introduction to the Theory of Computation*.  This is a somewhat
naive implementation, but it's straight out of the book, if not entirely
straight out of the mathematics.  See the comments in src/SimpleDfa.hs
for the details on how engineering expedience overrode mathematical 
meticulousness.

The Rust version is a bit wonkier than the Haskell version.  Rust is not
a lazy language, and so the nodes have to be made before we can link
them up, a process which is both time-consuming and error prone.  This
particular versione uses a Vec as the heap for the DFA; for immutable
DFAs this is efficient in time and space and guarantees that the entire
data structure will be collected when the Dfa handle is dropped, but
unlike the Haskell version this gives the developer twice as many
opportunities to point a particular node in the wrong direction.

Nonetheless, I'm pleased with how easily the Sipser DFAs in chapter one
of his book translate to Rust.  Certainly a hell of a lot easier than in
C or C++

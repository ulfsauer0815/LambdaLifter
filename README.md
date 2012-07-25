#LambdaLifter

##Description
An interactive version of the ASCII-Game *Lambda Lifting* as described in the **ICFP PC 2012** task.
You can find more information on the official site.

- [ICFP Programming Contest 2012 Official Site] (http://icfpcontest2012.wordpress.com/)
- [Full specification] (http://www-fp.cs.st-andrews.ac.uk/~icfppc/task.pdf)

###Extensions

A few specification extensions were posted:
- [Weather] (http://www-fp.cs.st-andrews.ac.uk/~icfppc/weather.pdf) *(not implemented yet)*
- [Trampolines] (http://www-fp.cs.st-andrews.ac.uk/~icfppc/trampoline.pdf)
- [Beards and Razors] (http://www-fp.cs.st-andrews.ac.uk/~icfppc/beards.pdf)
- [Higher Order Rocks] (http://www-fp.cs.st-andrews.ac.uk/~icfppc/horocks.pdf)

###Currently missing
- scoring (global)
- saving game progress

##Installation
Install required libraries

    $ cabal install mtl ansi-terminal

Compile source

    $ ghc Main.hs

Run with maps as arguments, e.g.

    $ ./Main ../maps/*.map


##Development notes
- This is my first "real" Haskell program, so there is probably a lot of code that will make an experienced Haskell user shiver ;)
- No proper (inline) documentation yet (*haddock* seems to be the universally accepted documentation tool, I have to look into that..)
- No consistent (indentation) style - still getting used to haskell.
- Almost no usage of appropriate monads.
- No proper parsing (e.g. metadata for the extensions) - i may read up on *parsec*.
- Chaotic code structure - how to structure and use modules doesn't seem to be as obvious in FP as it is in OOP - coupled with FP/Haskell being very new to me...

**Any sort of advice or criticism is very much appreciated!**
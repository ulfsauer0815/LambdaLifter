#LambdaLifter

##Description
An interactive version of the ASCII-Game *Lambda Lifting* as described in the **ICFP PC 2012** task.
You can find more information on the official site.

- [ICFP Programming Contest 2012 Official Site] (http://icfpcontest2012.wordpress.com/)
- [Full specification] (http://www-fp.cs.st-andrews.ac.uk/~icfppc/task.pdf)

###Extensions

- [Weather] (http://www-fp.cs.st-andrews.ac.uk/~icfppc/weather.pdf)
- [Trampolines] (http://www-fp.cs.st-andrews.ac.uk/~icfppc/trampoline.pdf)
- [Beards and Razors] (http://www-fp.cs.st-andrews.ac.uk/~icfppc/beards.pdf)
- [Higher Order Rocks] (http://www-fp.cs.st-andrews.ac.uk/~icfppc/horocks.pdf)

##Installation
Install Haskell and cabal ("Haskell package manager"), e.g.

    $ sudo apt-get install ghc cabal-install
    $ cabal update

Install required libraries

    $ cabal install mtl ansi-terminal

Compile source

    $ ghc Main.hs

Run with maps as arguments, e.g.

    $ ./Main ../maps/*.map

**Any sort of advice or criticism is very much appreciated!**
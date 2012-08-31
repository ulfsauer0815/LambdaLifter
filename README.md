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

##Screenshots

![Screenshot1](http://cloud.github.com/downloads/UlfS/LambdaLifter/ll4.png "Screenshot1")

![Screenshot2](http://cloud.github.com/downloads/UlfS/LambdaLifter/ll5.png "Screenshot2")

![Screenshot3](http://cloud.github.com/downloads/UlfS/LambdaLifter/ll2.png "Screenshot3")

![Screenshot4](http://cloud.github.com/downloads/UlfS/LambdaLifter/ll0.png "Screenshot4")

##Installation
Install Haskell and cabal ("Haskell package manager"), e.g.

    $ sudo apt-get install ghc cabal-install
    $ cabal update

Install required libraries

    $ cabal install mtl ansi-terminal data-lens

Compile source

    $ ghc Main.hs

Run with maps as arguments, e.g.

    $ ./Main ../maps/*.map


## Troubleshooting

If you have trouble compiling with the data-lens package, check if you have multiple *data-lens* packages installed and unregister other versions - this version has been tested with *data-lens-2.10.0*.

Listing installed *data-lens* packages

    $ ghc-pkg list | grep data-lens

Unregistering a package, e.g data-lens-2.10.4

    $ ghc-pkg unregister data-lens-2.10.4

**Any sort of advice or criticism is very much appreciated!**
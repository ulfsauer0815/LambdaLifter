# LambdaLifter

## Description
An interactive version of the ASCII-Game *Lambda Lifting* as described in the **ICFP PC 2012** task.
You can find more information on the official site.

- [ICFP Programming Contest 2012 Official Site] (http://icfpcontest2012.wordpress.com/)
- [Full specification] (http://www-fp.cs.st-andrews.ac.uk/~icfppc/task.pdf)

### Extensions

- [Weather] (http://www-fp.cs.st-andrews.ac.uk/~icfppc/weather.pdf)
- [Trampolines] (http://www-fp.cs.st-andrews.ac.uk/~icfppc/trampoline.pdf)
- [Beards and Razors] (http://www-fp.cs.st-andrews.ac.uk/~icfppc/beards.pdf)
- [Higher Order Rocks] (http://www-fp.cs.st-andrews.ac.uk/~icfppc/horocks.pdf)

## Screenshots

![Screenshot1](http://cloud.github.com/downloads/UlfS/LambdaLifter/ll4.png "Screenshot1")

![Screenshot2](http://cloud.github.com/downloads/UlfS/LambdaLifter/ll5.png "Screenshot2")

![Screenshot3](http://cloud.github.com/downloads/UlfS/LambdaLifter/ll2.png "Screenshot3")

![Screenshot4](http://cloud.github.com/downloads/UlfS/LambdaLifter/ll0.png "Screenshot4")

## Installation
Install Haskell and cabal ("Haskell package manager"), e.g.

    $ sudo apt-get install ghc cabal-install
    $ cabal update

### Installation

    $ cd LambdaLifter/
    $ cabal install

Cabal will print the path to the executable (usually */home/$USER/.cabal/bin/*).
If that directory is set in your PATH environment variable, you can just run

    $ lambdalifter

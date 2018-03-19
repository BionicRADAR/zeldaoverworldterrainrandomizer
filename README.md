# zeldaoverworldterrainrandomizer
A program that procedurally generates an overworld in a Legend of Zelda rom.
It does it in a sort of shuffling fashion, but it often doubles some elements and removes others, and it has to modify each screen so they fit together.

For more information, see [its github page](https://bionicradar.github.io/zeldaoverworldterrainrandomizer/).

Currently only alters terrain and puts overworld heart container where it belongs, and, to help avoid issues involving nonexistent caves, sets all cave entrances to dungeon 1. This means that exiting the cave on the fourth screen from the left on the bottom row will send you back to start. Does not alter Link's start position, which could cause a problem if the screen there has that position blocked.
Also does not alter monster spawns, which means certain screens can crash if a cave is exited there due to not having places to spawn monsters.

Requires a Legend of Zelda (US) rom (with 16 bytes of header) called "zelda-rom.nes" in the directory to run.

To use:
1. If you don't have them, [get common lisp](https://clisp.sourceforge.io/) and [install Java](https://www.java.com/en/download/help/download_options.xml) so that it can be run from the command line ([this may require changing the system path variable](https://introcs.cs.princeton.edu/java/15inout/windows-cmd.html)).

2. Get a The Legend of Zelda (US release, PRG0) rom; put it in the directory with zelda-overworld.lisp and name it "zelda-rom.nes"

3. Compile ZeldaDisplay.java (open a terminal in the zeldadisplay directory, then enter "javac ZeldaDisplay.java")

4. Open a terminal in the directory with zelda-overworld.lisp and start the Lisp interpreter by entering "clisp"

5. Enter "(load "zelda-overworld")" to load the program into the interpreter.

6. To make only a map image, enter "(make-map-only filename)", where "filename.png" is the name of the output file (it will appear in the directory with zelda-overworld.lisp).  
To make only a rom, enter "(make-rom-only filename)", where "filename.nes" is the name of the output rom.  
To make both, enter "(make-rom-and-map filename)", where filename.nes is the name of the rom, and filename.png is the name of the map.


Written almost entirely in GNU Common Lisp 2.49; visit [clisp.org](https://clisp.sourceforge.io/) for more information and to download an interpreter. The image-making part of the program was written in Java due to its convenient image-manipulation libraries that I was already familiar with.


This program was inspired by the [Zelda Randomizer Project](https://sites.google.com/site/zeldarandomizer/), a program that randomizes just about everything in The Legend of Zelda except for the overworld terrain. Unlike this program, the Zelda Randomizer's output is actually very fun if you've already played through the original game.

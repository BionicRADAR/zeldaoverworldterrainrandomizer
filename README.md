# zeldaoverworldterrainrandomizer
A program that procedurally generates an overworld in a Legend of Zelda rom.
It does it in a sort of shuffling-like fashion, but it often doubles some elements and removes others, and it has to modify each screen so they fit together.

Currently only alters terrain and puts overworld heart container where it belongs, and, to help avoid issues involving nonexistent caves, sets all cave entrances to dungeon 1. This means that exiting the cave on the fourth screen from the left on the bottom row will send you back to start. Does not alter Link's start position, which could cause a problem if the screen there has that position blocked.
Also does not alter monster spawns, which means certain screens can crash if a cave is exited there due to not having places to spawn monsters.
Short-term fixes are planned for the above issues.

Requires a Legend of Zelda (US) rom with one line of header called "zelda-rom.nes" in the directory to run.

Written in GNU Common Lisp 2.49; visit clisp.org for more information and to download an interpreter.

Miscellany
==========

A miscellany of programs I've written that are too small to deserve repositories
of their own. Most of these took under an hour to write.

Brainfuck.hs
------------

This is an interpreter for the Brainfuck language, written in Haskell. I just
wanted to see what writing something like this would be like.

Huffman.hs
----------

This is a simple Huffman encoder/decoder that I wrote when I learned about
Huffman encoding. It's not particularly useful. Also written in Haskell.

SAT.hs
------

This is a simple SAT solver, written in Haskell. It takes lines from stdin, and
parses and solves them. It's not very good, it's not fast and it doesn't handle
incorrect input. I mostly wrote this so I could practice writing Pratt parsers.

XMoconk.hs
----------

My desktop runs XMonad, and has a dzen2 bar to display window and workspace
information, hardware monitors, and a clock. This is a problem, because XMonad
only responds to X events, which means you can't set it to update the clock and
hardware monitors at regular intervals. Most people in this situation resort to
either switching to XMobar, or using two dzen2 bars side by side. I didn't like
either of those options, so instead I wrote this program.

XMoconk takes a line from stdin, parses it into a space-separated list of ten
integers, packs them into an X event and sends it, and repeats, forever. In my
XMonad config, I have an event hook that intercepts these events and uses them
to update the dzen2 bar. Then, I simply pipe Conky to XMoconk. Since Conky
outputs new hardware data at regular intervals (in my case, once per second),
XMonad will get regular X events from XMoconk, and will update the dzen2
instance accordingly.

XLogout.hs
----------

I don't know if there's an easy way to logout of XMonad from a terminal. I
couldn't find one, so I wrote this. When you run this program, it sends an X
event. In my XMonad config, I have an event hook that intercepts this event and
tells XMonad to logout.

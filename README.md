Boggle solver
=============

A long time ago I have found a Boggle game on my linux box. Nowadays you can find this game in bsdgames package or bsd-games depending on the linux you are using (haven't researched how to get it to run on other OSs).

The game itself is pretty easy. You are given a 4x4 matrix with charaters in each tile. Characters may repeat, they are just randomly placed there, no fancy stuff when generating a new board. Your job is to find as many words in this matrix as possible, there is a time limit of 3 minutes to do so. The word can be constructed from characters adjacent to each other (horizontally, vertically, diagonally), but the same tile cannot be used more than once during the word construction.

Boggle solver is a simple solution for this game. It takes a dictionary of valid words (which I took from /usr/share/dict/words on my linux box) and tries to find each of the word on the board (well, some filtering before happens as well to optimize the solution, although it is not complete, more optimization could be done). My major goal was to use State monad in this project for searching a word in the matrix. I learned quite a bit by using it here.

    cabal sandbox init
    cabal install --dependencies-only
    cabal build
    cabal run -- kwedeeofpjstkatd

Here are the screenshots of the solver in action (*screenshots were made before moving project to cabal*):

![Boggle solver at work](http://ksaveljev.github.io/boggle-solver.png)

![Boggle game from bsdgames](http://ksaveljev.github.io/boggle.png)

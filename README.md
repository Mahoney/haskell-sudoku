# Haskell Sudoku

A little toy to start writing something real in Haskell.

### To Build & Run
#### Checkout
```
git clone git@github.com:Mahoney/haskell-sudoku.git && \
cd haskell-sudoku
```
#### In Docker
```
./do build

./do run ......3..6...2.1...328.7.......4..6.4.69.85.7.9..7.......1.582...4.8...1..5......
```
or
```
./do buildAndRun ......3..6...2.1...328.7.......4..6.4.69.85.7.9..7.......1.582
...4.8...1..5......
```
#### Natively
```
git clone git@github.com:Mahoney/haskell-sudoku.git && \
cd haskell-sudoku && \
stack setup && \
stack build

~/.local/bin/haskell-sudoku ......3..6...2.1...328.7.......4..6.4.69.85.7.9..7.......1.582...4.8...1..5......
```

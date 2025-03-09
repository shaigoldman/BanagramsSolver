# BanagramsSolver
Dictionary from http://wordlist.sourceforge.net/

To build the project, run:

```$ stack build```

For Parallel, run using this (replace "4" with the number of cores):

  ```$ stack exec BananaSolver-exe -- +RTS -ls -N4 -- <algo> <tiles> <algo> must be 's' for sequential or 'p' for parallel. Tiles must be lowercase letters.```

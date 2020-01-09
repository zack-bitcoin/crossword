Crossword
=========

This is a tool that can generate many crossword puzzles.
It uses definitions from an old dictionary as the hints.
Has more than 10k words with different spellings.
Has around 50k different definitions, some words have multiple definitions.


Example
==============

example of generating the hints for one of the puzzles stored on 4_5_puzzles.txt:
```
erl
c(crossword).%this compiles the program. you only need to do it once.
c(puzzle_maker).%this compiles the program. you only need to do it once.
D = crossword:load_dict().%this takes several seconds.
[PuzzleText, Hints] = puzzle_maker:make_4_5(D).

[{<<"slamhaleabitrearelse">>,
  [<<"slam">>,<<"hale">>,<<"abit">>,<<"rear">>,<<"else">>],
  [<<"share">>,<<"label">>,<<"alias">>,<<"metre">>]},
 {[<<"The act of one who, or that which, slams.">>,
   <<"Welfare.">>,<<"3d sing. pres. of Abide.">>,
   <<"To place in the rear; to secure the rear of.">>,
   <<"Otherwise; in the other, or the contrary, case; if the facts were different.">>],
  [<<"The pubes; the sharebone.">>,
   <<"To affix a label to; to mark with a name, etc.; as, to label a bottle or a package.">>,
   <<"A second or further writ which is issued after a first writ has expired without effect.">>,
   <<"A measure of length, equal to 39.37 English inches, the standard of linear measure i"...>>]}]
6> 
```

to quit the erl shell: `halt().`

Navigating the files
==========

[Here are the 20 000 most common words, according to google](20k.txt)

[Here are the words from a dictionary from 1913. It is on project Gutenburg](dictionary2.csv)

[here are the 4x5 puzzles in a plain text data file](4_5_puzzles.txt) other files in the same folder contain more puzzles. Does not contain hints.

[this program](puzzle_maker.erl) is able to choose a random puzzle from 4_5_puzzles.txt, and generate the hints for it.

[this program](crossword.erl) is able to look at the 20k words from google, find the subset of them that are defined in the dictionary from 1913, and then generate all possible crossword puzzles for a given board using those words.

example of using crossword to find all combinations of words that make valid boards:
```
c(crossword).%this compiles the program. you only need to do it once.
D = crossword:load_dict().
Boards = crossword:word_combos(D).
```

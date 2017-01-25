[![Build Status](https://travis-ci.org/andsild/TextToNumber.svg?branch=master)](https://travis-ci.org/andsild/TextToNumber)
# TextToNumberParser
Something of an overkill to transform text to numbers.

| Input                         | Output        |
| ------------------------------|:--------------|
| 264                           | 264           |
| Two hundred 64                | 264           |
| Two hundred and 64            | 264           |
| Two hundred and sixty 4       | 264           |
| Two hundred and sixty-four    | 264           |

## Note:
Its one of my first haskell programs. I've seen one-liners that work for e.g. roman numerals so I realize theres a lot of room for improvement.  

When I have time I'll work on the following improvements:
* Floating point numbers
* Two consequtive numbers after each other should be parsed individually (5000 3000 should yield 5000 3000 not a combination of the two)

# TextToNumberParser
Something of an overkill to transform text to numbers.

| Input                         | Output        |
| ------------------------------|:--------------|
| 264                           | 200           |
| Two hundred 64                | 264           |
| Two hundred and 64            | 264           |
| Two hundred and sixty 4       | 264           |
| Two hundred and sixty-four    | 264           |

## Note:
Its one of my first haskell programs. I've seen one-liners that work for e.g. roman numerals so I realize theres a lot of room for improvement.  

When I have time I'll work on the following improvements:
* parsing numbers as a CFG, not a string-lookup table
* parsing multiple numbers in one sentence ("one hundred and five chickens met 50 cows" should return "105, 50", not 155) 

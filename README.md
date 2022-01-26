# wordlizer

Simple tool to find candidate [wordle](https://www.powerlanguage.co.uk/wordle/) solutions from `/usr/dict/words`

## Example

```
❯ wordlizer P[e]ac[e]
packs
pacts
panic
Suggested guesses: (1.0 on average)
 - packs
 - pacts
```

## Execute

* Run `stack exec -- wordlizer --help` to see the following usage information:

```
wordlizer - help yourself seem smart

Usage: wordlizer [--version] [--help] [?????] [-n|--not CHARS] [--words FILE] 
                 [--verbose] [--max-candidates INT]
  Simple tool to find candidate wordle solutions from /usr/dict/words

Available options:
  --version                Show version
  --help                   Show this help text
  ?????                    Known parts, including A (correct) and a (misplaced)
                           letters, e.g. ?Ab?c
  -n,--not CHARS           Characters that the word must not have (default: "")
  --words FILE             File containing word list
                           (default: "/usr/share/dict/words")
  --max-candidates INT     Maximum number of candidates to show (default: 125)
```

## Run tests

`stack test`

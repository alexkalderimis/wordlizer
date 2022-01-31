# wordlizer

Simple tool to find candidate [wordle](https://www.powerlanguage.co.uk/wordle/) solutions from `/usr/dict/words`

## Example

```shell
❯ wordlizer solve P[e]ac[e]
packs
pacts
panic
Suggested guesses: (1.0 on average)
 - packs
 - pacts
```

## Execute

* Run `wordlizer --help` to see the following usage information:

```shell
❯ wordlizer --help
wordlizer - help yourself seem smart

Usage: wordlizer [--version] [--help] [--words FILE] [--dictionary FILE] 
                 [--verbose] [--max-candidates INT] COMMAND
  Simple tool to find candidate wordle solutions from /usr/dict/words

Available options:
  --version                Show version
  --help                   Show this help text
  --words FILE             File containing word list
                           (default: "/usr/share/dict/words")
  --dictionary FILE        File containing word list
                           (default: "/usr/share/dict/words")
  --max-candidates INT     Maximum number of candidates to show (default: 40)

Available commands:
  solve                    Use guesses to solve the puzzle
  appraise                 Appraise the quality of a guess
  play                     Play a game
```

## Run tests

```shell
stack test
```

## Installation

Install to somewhere sensible (e.g. `~/.local/bin`):

```shell
stack install
```

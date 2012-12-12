TySh
====

Typed Shell with structured data representation.

## Useful links

- <http://book.realworldhaskell.org/read/systems-programming-in-haskell.html>
- <http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours>
- <http://damnkids.posterous.com/rich-format-unix-pipes>
- <http://www.dwheeler.com/essays/fixing-unix-linux-filenames.html>
- <https://news.ycombinator.com/item?id=4368993>
- <http://stuff.mit.edu/afs/sipb.mit.edu/user/yandros/doc/es-usenix-winter93.html>
- <https://github.com/pkamenarsky/ytools>

### Parsing

- <http://legacy.cs.uu.nl/daan/download/parsec/parsec.html>
- <http://book.realworldhaskell.org/read/using-parsec.html>

## Examples - piping/currying between built-in utilities
```bash
TySh> ls | take 4
TySh> ls | sort | pick 3
TySh> read test/1.txt | write test/new.txt
TySh> read test/new.txt | set TEST
```

## Examples - piping/currying between built-in and system utilities
```bash
TySh> read test/1.txt | tr a-z A-Z
TySh> date | set TEST
TySh> date | tr a-z A-Z
```

## Demo
```bash
[baldur@ed-3358-14 TySh]$ runghc Tysh.hs 
Welcome to TySh! Use 'quit' or 'q' exit the shell.
:TySh> get USER
baldur
0:TySh> set PS1 "Example> "
0:Example> cd /tmp
0:Example> get PWD
/tmp
0:Example> pwd
/tmp
0:Example> ls
<file> ...
0:Example> ls | take 5
orbit-forerik cupsla4yE0 cups9LF16X cupsloKtuu host_0
0 0:Example> ls | take 5 | sort 
cups9LF16X cupsla4yE0 cupsloKtuu host_0 orbit-forerik
0 0 0:Example> ls | take 5 | sort | write ls-output
0 0 0 0:Example> read ls-output
cups9LF16X cupsla4yE0 cupsloKtuu host_0 orbit-forerik
0:Example> date 
Wed Dec 12 10:42:29 CET 2012
0:Example> date | set today'sdate
0 0:Example> get today'sdate
Wed Dec 12 10:42:38 CET 2012
0:Example> get today'sdate | tr a-z A-Z
WED DEC 12 10:42:38 CET 2012
0 0:Example> get todays'date | take 5 | write testing
usage: write file input
0 2 2:Example> get PIPESTATUS
0 2 2
0:Example>
```
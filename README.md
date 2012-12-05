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

## Example
```bash
TySh> cd /tmp
Pipe [Command "cd" ["/tmp"] []]

TySh> pwd
Pipe [Command "pwd" [] []]
/tmp

TySh> date | tr a-z A-Z
Pipe [Command "date" [] [],Command "tr" ["a-z","A-Z"] []]
TUE DEC  4 23:40:30 CET 2012

TySh> get PATH | xargs -n1 | tr a-z A-Z
Pipe [Command "get" ["PATH"] [],Command "xargs" ["-n1"] [],Command "tr" ["a-z","A-Z
/CHALMERS/USERS/-----/.CABAL/BIN
/BIN
/USR/BIN
/USR/SBIN
...

TySh> set PS1 shell>
Pipe [Command "set" ["PS1","shell>"] []]

shell> 
```



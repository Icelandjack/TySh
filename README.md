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

## Example
```bash
TySh> set SHELL TySh
Pipe [Command "set" ["SHELL","TySh"] []]

TySh> get SHELL | tr a-z A-Z
Pipe [Command "get" ["SHELL"] [],Command "tr" ["a-z","A-Z"] []]
JUST TYSH
TySh> cd /tmp
Pipe [Command "cd" ["/tmp"] []]

TySh> date | tr a-z A-Z
Pipe [Command "date" [] [],Command "tr" ["a-z","A-Z"] []]
TUE DEC  4 23:40:30 CET 2012

TySh> set PS1 shell>
Pipe [Command "set" ["PS1","shell>"] []]

shell> 
```



Commands
========

All the available key commands are in this directory.  A key command can really do whatever it wants, with the only restriction being that it must provide the symbol 'data' as a list.  For each command, add the key and function to the list, as in the following example: `(define data (#\/ search 'search handle-search))`

The api for accessing the screen and/or graph is currently undocumented.


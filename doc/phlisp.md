phlisp
======

Philosophy
----------

Phlisp is a lisp-like language that is defined as a directed, partially ordered, labeled graph (hereafter the "abstract syntax graph" or "ASG").  Phlisp's main influences are members of the lisp family, particularly Racket.  The purpose of Phlisp is to create a language that may be edited not with a text editor but with a tree/graph editor built for the purpose.  The canonical implementation is phlisped.

Phlisp code compiles to Racket code, which may then be run with the standard Racket utilities.  Most phlisp code translates fairly directly to Racket.  The main functional additions are related to defining variables.  Instead of using `define` and `let` statements, we have each reference to a variable simply reference the same node in the ASG.  These variables are assigned a particular environment node and may be referenced in any lexical descendant of that node.  In this way, all cross references are direct, and variable handling is greatly simplified.

Additionally, functions are defined in a manner very similar to variables.  The main differences are obviously that functions are evaluated explicitly when they are referenced rather than implicitly at the level of the environment node and functions may have parameters.  Note that for a pure function, the only difference is that functions may have parameters (and so variables are merely the zero-parameter special case of functions).  In practice, though, mutating state is a fact of life, so we maintain the distinction.  Thus, in phlisp, we mark variables as functions rather than using `lambda` explicitly.

The Graph
---------

All nodes in the graph are distinguishable.  A node must be exactly one of the following:

- a parent of one or more nodes
- a variable defined as another node
- an argument to a function
- a terminal node

### Parent Nodes

Parent nodes must have one or more forward edges to a parent, variable, or terminal node labeled `has-child`.  They may have one or more forward edges to variable nodes labeled `is-environment-of`.  Additionally, they may have a forward edge to a string labeled `has-name`.

### Variable Nodes

Variable nodes have exactly one forward edge to a parent, variable, terminal, or argument node labeled `is-defined-as`.  Additionally, they may have a forward edge to a string labeled `has-name`.  They may have a forward edge to `-1` labeled `is-function`.  If so, then we call it a function node, and it may have one or more forward edges to an argument node labeled `has-formal-arg`.

### Argument Nodes

Argument nodes have exactly one forward edge to `-1` labeled `is-formal-arg`.  Additionally, the may have a forward edge to a string labeled `has-name`.

### Terminal Nodes

Terminal nodes must have a forward edge to a string labeled `has-name`.

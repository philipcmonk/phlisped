phlisp
======

Philosophy
----------

Phlisp is a lisp-like language that is defined as a directed, partially ordered, labeled graph (hereafter the "abstract syntax graph" or "ASG").  Phlisp's main influences are members of the lisp family, particularly Racket.  The purpose of Phlisp is to create a language that may be edited not with a text editor but with a tree/graph editor built for the purpose.  The canonical implementation is phlisped.

Phlisp code compiles to Racket code, which may then be run with the standard Racket utilities.  Most phlisp code translates fairly directly to Racket.  The main functional additions are related to defining variables.  Instead of using `define` and `let` statements, we have each reference to a variable simply reference the same node in the ASG.  These variables are assigned a particular environment node and may be referenced in any lexical descendant of that node.  In this way, all cross references are direct, and variable handling is greatly simplified.

Additionally, functions are defined in a manner very similar to variables.  The main differences are obviously that functions are evaluated explicitly when they are referenced rather than implicitly at the level of the environment node and functions may have parameters.  Note that for a pure function, the only difference is that functions may have parameters (and so variables are merely the zero-parameter special case of functions).  In practice, though, mutating state is a fact of life, so we maintain the distinction.  Thus, in phlisp, we mark variables as functions rather than using `lambda` explicitly.

The Graph
---------

All nodes in the graph are distinguishable.  All lists are assumed unordered unless explicitly specified as ordered.  All nodes must have a name.  A node must be exactly one of the following:

- a parent of one or more nodes
- a variable defined as another node
- an argument to a function
- a terminal node

### Parent Nodes

Parent nodes must have one or more ordered parent, variable, or terminal node children.  They may have one or more variable node vars.

### Variable Nodes

Variable nodes have exactly one parent, variable, terminal, or argument node definition.  They may be labeled `is-function`.  If so, then we call it a function node, and it may have one or more ordered argument node arguments.

### Argument Nodes

Argument nodes must be labeled as such.

### Terminal Nodes

Terminal nodes must be labeled as such.

Compilation
-----------

The compilation of any node is dependent only on the lexical children of that node.  Compilation follows the following algorithm.

Define `c(n)` such that if `n` is a:

- variable node, then if `id` is a unique symbol associated with `n`, yield `id`.
- terminal node, then if `n` has name `name`, yield `name`.
- parent node, let `n` have `r` child nodes labeled `child1 child2 ... childr` and `s` vars `var1 var2 ... vars` with unique associated symbols `id1 id2 .. ids`.  For each `varp`, let `resp` be `c(varp)` if `varp` is not a function node, else let `varp` have `t` formal arguments labeled `arg1 arg2 ... argt` with unique associated symbols `idp1 idp2 ... idpt` and let `resp` be `(lambda (idp1 idp2 ... idpt) c(varp))`  yield `(letrec ((id1 res1) (id2 res2) ... (ids ress)) (c(child1) c(child2) ... c(childs)))`.  Note that since vars is unordered, to get the ordering, we topologically sort the ends such that if the definition of `varp` references `varq` in any place other than a function definition, then `varp` precedes `varq`.

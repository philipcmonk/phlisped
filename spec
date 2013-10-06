Copyright Philip Monk, August 2013, All rights reserved.

Language
========

A program is a directed, labeled, partially ordered graph.

The triples have an ordering.  Although implemented as a total ordering, the only comparisons that should be relied upon are those with the same start and edge values.  The ordering of backward edges is not defined.  The only edges where this is applicable are the "has child", "has arg", and "has formal arg".  All other edges are singletons, i.e. for each start there must be no more than one triple with any given edge.

"is" may only be used with start next-id.

If alice is defined as bob, then bob must have env.

If unexpected edges exist, they should be ignored.

Edges:
        "has child"
        "has arg"
        "has formal arg"
	"is defined as"
	"is named"
	"is written"
	"is reified as"
	"is call to"
	"has scope"
	"has env"
	"is"

May (x) or must (Y) be used in conjunction with: (Y means if the row-edge is present, then the column-edge must be present, but the converse is not necessarily true)

	hc	ha	hfa	ida	in	iw	ira	ict	hs	he	is
hc	x		x		x				x
ha		x	x		x			Y	x
hfa	x	x	x	x	x	x		x	Y	
ida			x		x				x	Y
in	x	x	x	x				x	x	x
iw			x				x		x
ira						Y
ict		x	x		x				x
hs	x	x	x	x	x	x		x
he			x	x	x
is

	hc	hfa	ida	in	iw	ira	he	if	is
hc	x			x			
hfa		x	Y	x			Y	Y
ida		x		x			Y	x
in	x	x	x				x	x
iw						x
ira					Y
he		x	x	x				x
if		x	Y	x			x
is

Language implementation
=======================

The program graph is implemented as a hash where each entry has the form (id --> (l1 . l2)) where l1 is a list of the edges starting at id and l2 is a list of the edges ending at id.

graph-neighborhood-{forward,backward} returns {l1,l2}, graph-neighborhood-edge-{forward,backward} returns {l1,l2} filtered for the requested edge.

For all modifications, we must preserve the ordering of each l1.  Right now, l2 is in order of date added, but this should not be depended on.

graph-append-edge{s} returns a new hash with e{s} appended to the l1 associated with (triple-start {(car }e{)}) and added to the l2{s} associated with (triple-end e{s}).

graph-prepend-edge{s} returns a new hash with e{s} prepended to the l1 associated with (triple-start {(car }e{)}) and added to the l2{s} associated with (triple-end e{s}).

graph-replace-edges returns a new hash with es replacing e in the l1 associated with (triple-start e) and e removed from the l2 assoiciated with (triple-end e) and es added to the l2{s} associated with each of (triple-end es).  Note that all es must have the same triple-start.

graph-remove-edge returns a new hash with e removed from the l1 associated with (triple-start e) and the l2 associated with (triple-end e).

Trees
=====

Trees is a list of whole-tree.

The first element of Trees must be a dummy tree, the second element must be the Bar-tree, and there must always be a third element.  The contents of the dummy tree should never have a functional impact.

Selected-tree
=============

Selected-tree must always point to a valid tree.

Bar-tree
========

Bar-tree must always point to a valid tree.

whole-tree
==========

A whole-tree contains all the data necessary to display a tree on the screen.

n-tree is a valid node.

childfunc is a function that takes one argument (namely, the data of a particular node), and returns the children of this node in the current tree.  This function should return the same value each time it is called with the same input.  If this function is changed, the n-tree and utterance-tree should be regenerated and the selection and open set cleared.

utterance-tree is a tree of utterances.  utterance-node should provide an injective mapping from the utterances in utterance-tree to the nodes in n-tree.

open is a set of laddrs.  Each laddr must correspond to a valid utterance.

selection is a laddr.  selection must correspond to a valid utterance.

x y w h offset-x offset-y zoom are numbers.  zoom must be positive.

utterance
=========

An utterance contains all the data necessary to display a single node.

node is a valid node.

x y w h text-w text-h are nonnegative numbers.

args is an eargerly-evaluated list of utterances.  An utterance may not be its own descendant.

clr is a pair of the form (f . b) where f is a foreground color and b is a background color.  Both f and b are lists of length three representing rgb on a scale of 0-255.

runtime-vals
============

runtime-vals is a mutable hash from ids to a list of values.  The order of the values is such that as soon as a value for a particular id is known, it is added to the end.  Thus, it is in order of _exits_, not _entrances_ into a node.  If we want to change that, then we'd need to come up with a way to put in placeholders as we're stepping through and at the end replace them with the value.  This is very doable, I'm just not sure if it's the correct design decision.

visualization
=============

A visualization must implement the following functions:

* paint-tree
* node->v11n-utterance
* wheel {'left,'right,'up,'down}
* find-utterance

A visualization may implement the following functions:

* zoom

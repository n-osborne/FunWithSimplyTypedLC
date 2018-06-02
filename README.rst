lambda Typer <°λ°>
==================

Generated doc is availble `here <https://n-osborne.github.io/FunWithSimplyTypedLC/>`_.

Description
-----------

``lambda Typer``  (``lTyper`` for short) provides tools to generate a tree-like
description of the type of a lambda expression.

Installation
------------

First, clone or download the repository:

.. code::
   
   git clone https://github.com/n-osborne/lTyper

The easiest way to build and install ``lTyper`` is ``cabal``. At the root of the
repository, type the following instructions:

.. code::
   
   cabal configure
   cabal build
   cabal install

If you don't want to use ``cabal``, you can compile by hand using ``ghc``. Place
yourself in the ``src`` directory and run the instructions corresponding to the
executables you want:

.. code::

   ghc Exp2Tree.hs -o exp2tree
   ghc File2Tree.hs -o file2tree
   
That will generate the corresponding executables ``exp2tree`` and ``file2tree``.
You may want to put them in your path.

User guide
----------

``exp2tree`` takes two arguments on the command line. The first is the filename
for the result, the second is the lambda expression to parse. As the syntax we
use is whitespace separated, the second argument should be put between quotes
(otherwise, there would be as many arguments as words in the expression).

The new file created is ready to be used with the ``dot`` program from the
`Graphviz <http://graphviz.org/about/>`_ suite. 

For example, the following sequence of commands produce the tree below:

.. code::

   exp2tree makePairOfChar "apply lambda x:Char apply lambda y:Char pi x:Char y:Char a b"
   dot -Tsvg makePairOfChar > makePairOfChar.svg

.. Image:: img/makePairOfChar.svg

``file2tree`` takes also two arguments, but they both are filename. The first is
the filename for the result, the second is the filename of the file the content
of which is the description of the lambda expression to parse.

The advantage of this second option over the first is that it allows to build
more complex expression, as the file can contains ``let`` instructions.

A source file for ``file2tree`` is composed of two sections deliminated by three
key words.

Between the word ``Assignations`` and the word ``Program`` there are
some ``let`` instructions defining some macro (some sort of preprocessing).

Between the word ``Program`` and the word ``End``, there is the expression to
parse.

Here is one of the example provided in the ``examples`` directory:

.. code::

   file2tree complex complex.stlc
   dot -Tsvg complex > complex.svg

.. Image:: img/complex.svg

	   
Brief description of the language
---------------------------------

We give ourself an infinite set of variables and we have the set
:math:`\mathbb{C}={zero, a\dots b, true, false, null}` of constants. A well
formed expression is defined by the following BNF:

.. math::
   E :=& var:type \\
   |& const\\
   |& lambda\; var:type\; E \\
   |& apply\; E_1\; E_2\; \textrm{(given }E_1:A\rightarrow B\textrm{ and }E_2:A\textrm{)}\\
   |& succ E\; \textrm{(given }E:nat\textrm{)} \\
   |& pi\; E_1\; E_2 \\
   |& first\; E\; \textrm{(given }E:A\times B\textrm{)} \\
   |& second\; E\; \textrm{(given }E:A\times B\textrm{)} \\

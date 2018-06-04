lambda Typer <°λ°>
==================

Generated doc is availble `here <https://n-osborne.github.io/lTyper/>`_.

Description
-----------

``lambda Typer`` (``lTyper`` for short) provides tools to generate a tree-like
description of the type of a lambda expression from the lambda calculus typed
with (one) type constructor. That would be :math:`\lambda \underline{\omega}`
in the `lambda cube <https://en.wikipedia.org/wiki/Lambda_cube>`_.

The syntax is quite simple.

The set of raw types consists of four elements: {nat, char, bool, empty}

We have an infinite set of variable names that must be typed (for example
``x:nat`` or ``y:char``).

The type ``char`` is populated by the 26 lower case letters of the latin alphabet.

- ``zero`` is the constant equal the the lower natural number.
- ``succ`` is the successor operator which take a ``nat`` as argument.
- ``pi`` is the type constructor for the product type (``pi`` stands for pair introduction).
- ``first`` extracts the first element of a pair.
- ``second`` extracts the second element of a pair.
- ``true`` is the constant for the boolean true.
- ``false`` is the constant for the boolean false.
- ``null`` is the only member of the type empty.
- ``lambda`` build a lambda abstraction. It is a binary operator that takes a
  typed name (variable) as a binder and a lambda expression as the body of the
  lambda abstraction. For example, ``lambda x:nat succ x:nat`` is the function
  that compute the successor of every natural number given as argument.
- ``apply`` is a binary operator that apply the second argument to the lambda
  abstraction which is the first argument. For example, ``apply lambda x:nat
  succ x:nat zero`` computes the successor of zero, that is one.
      
    

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


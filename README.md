random_access_list_-Haskell-
============================

Simple random access list (fast prototyping on Haskell for C++ language).

  It is random access list (ral) based on simplest self-balancing binary
  search tree with implicit keys.
  Code is pure functional.
  Tested by unit test framework QuickCheck.
  Project writed as fast prototyping for C++ language
  and it have many disadvantages for Haskell:
  * Not splited for files.
  * Haven't function signatures.
  * Not full covered by unit tests.
  * Not use selfdocumented code style.
  * ral interface functions not suited for a haskell style (without zippers, without change position of container parameter to last and others).
  * ral interface functions have collisions with functions from Data.List module and others.
  * ral interface not exported as module.
  * ral not derived and not instanced many required by Haskell type classes, for example "Foldable", "Monoid" and many others.
  * Not corrected strictness and laziness.
  * Functions not written with tail recursion, not optimized memory consumption of tree.
  * Not configured git:ignore list.


Use indices as iterators and it is not stable despite of iterators on ::std::list<T>.
Average and worst case operation complexity and memory consumption is O(log n) for all operations with one element.
Average operation complexity of traverse tree to next element and get its value is O(1) (I mean, traverse without using iterators).

RAL on C++ loses pure functional realization and have much changed interface.

Let see to MAIN.hs.

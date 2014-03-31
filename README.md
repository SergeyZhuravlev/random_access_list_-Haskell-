random_access_list_-Haskell-
============================

Simple random access list (fast prototyping for C++ language).

  It is random access list (ral) based on simplest self-balancing binary
  search tree with implicit keys.
  Code is pure functional.
  Tested by unit test framework QuickCheck.
  Project writed as fast prototyping for C++ language
  and it have many disadvantages for Haskell:
  * Not splited for files.
  * Haven't function signatures.
  * Not full covered by unit tests.
  * Not use selfdocumented code style some times.
  * ral interface functions not suited for a haskell style (without zippers, without change position of parameter to last and others).
  * ral interface functions have collisions with functions from Data.List module and others.
  * ral interface not exported as module.
  * ral not derived and not instanced many required by Haskell type classes, for example "Foldable", "Monoid" and many others.
  * Not coorected strictness and laziness.
  * Functions not writed with tail recursion some times.


Use indices as iterators and it is not stable despite of iterator at ::std::list<T>.
Average and worst case operation complexity and memory consumption is O(log n) for all operations with one element.
Average operation complexity of iteration to next element is O(1).

Let see to MAIN.hs.

# Reactive Streams

Implementation of many ReactiveX combinators in Haskell built on top of Edward
Kmett's [`machines`](http://hackage.haskell.org/package/machines) library. I
refrain from calling this an RxHaskell implementation because it snubs the idea
of Observables in favour of machines. Regardless, most of what you may want to
do with streams is possible using this library.

Most combinators are simply re-exports from
[`machines`](http://hackage.haskell.org/package/machines) with differing naming
conventions. All combinators are compatible with the Machines library.

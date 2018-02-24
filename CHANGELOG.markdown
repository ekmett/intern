next
----
* Add a `Semigroup IntSet` instance.

0.9
---
* Removed `identity` from the Interned class, to support applications where the identity is obtained by other means (e.g. a unique Ptr value)

0.8
---
* Disabled cache removal as it was causing problems on large data sets. There is no good way to ensure that both references remain alive long enough to finish comparisons.
* Switched to IORef from MVar

0.7
---
* Fixed problem where comparisons could happen between data structures while one was still a thunk, leading to equal structures comparing as inequal in limited circumstances, by appropriately using strictness annotations.

0.6
---
* Widened the caches so they don't go through a single MVar per type. This has made a dramatic impact on performance. However, this broke the previous invariant that newer entries always had higher Ids than older entries.

0.5.2
-----
* Added Data.Interned.IntSet

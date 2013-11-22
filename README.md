A Scala implementation of Concurrent Revisions
==============================================

This is an experiemental implementation of [Concurrent Revisions](http://research.microsoft.com/en-us/projects/revisions/).

> The Revisions project introduces a novel programming model for
> concurrent, parallel, and distributed applications. It provides
> programmers with a simple, yet powerful and efficient mechanism (based
> on mutable snapshots and deterministic conflict resolution) to execute
> various application tasks in parallel even if those tasks access the
> same data and may exhibit read-write or write-write conflicts.


References
----------

Sebastian Burckhardt, Alexandro Baldassion, and Daan Leijen.
	[Concurrent Programming with Revisions and Isolation Types.](http://research.microsoft.com/apps/pubs/default.aspx?id=132619)
	In _Proceedings of the ACM International Conference on Object Oriented Programming Systems Languages and Applications (OOPSLA’10)_,
	ACM SIGPLAN, Reno, NV, October 2010

Daan Leijen, Sebastian Burckhardt, and Manuel Fahndrich.
	[Prettier Concurrency: Purely Functional Concurrent Revisions.](http://research.microsoft.com/apps/pubs/default.aspx?id=151805)
	In _Haskell Symposium 2011 (Haskell’11)_,
	ACM SIGPLAN, Tokyo, Japan, 7 July 2011

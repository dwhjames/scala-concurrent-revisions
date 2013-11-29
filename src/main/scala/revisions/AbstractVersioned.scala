package revisions

import scala.collection.concurrent


abstract class AbstractVersioned[T] extends Versioned {
  protected val versions: concurrent.Map[Int, T] =
    new concurrent.TrieMap[Int, T]

  // a cache of the last read or write to this versioned object
  // the lower 16 bits contain the version number of the segment
  // the higher 16 bits contain the index of the value in the map
  @volatile
  protected var cache: Int = 0

  @inline
  final def get: T =
    get(Revision.currentRevision.get())
  @inline
  final def get(rev: Revision[_]): T =
    get(rev.current)

  final protected def get(seg: Segment): T = {
    val c = cache
    if (seg.version == (c & 0xFFFF)) {
      versions(c >>> 16)
    } else {
      var s = seg
      while (!versions.contains(s.version)) {
        s = s.parent
      }
      cache = ((s.version << 16) | (seg.version & 0xFFFF))
      versions(s.version)
    }
  }

  final protected def set(rev: Revision[_], x: T): Unit = {
    val seg = rev.current
    val v = seg.version
    if (!versions.contains(v)) {
      seg.written += this
    }
    cache = ((v << 16) | (v & 0xFFFF))
    versions(v) = x
  }

  // release the value stored for a segment
  final override def release(release: Segment): Unit =
    versions.remove(release.version)

  // collapse the value in the parent segment into the main revision
  final override def collapse(main: Revision[_], parent: Segment): Unit = {
    if (!versions.contains(main.current.version)) {
      // if not already written in main, copy from parent
      set(main, versions(parent.version))
    }
    // release value for parent
    versions.remove(parent.version)
  }

  protected def computeMerge(main: Revision[_], joinRev: Revision[_], join: Segment): T

  final override def merge(main: Revision[_], joinRev: Revision[_], join: Segment): Unit = {
    require(versions.contains(join.version))

    // walk back up the segment history of the revision to be joined
    var s: Segment = joinRev.current
    while (!versions.contains(s.version)) {
      // while this value does not have a version in the segment
      s = s.parent
    }
    if (s eq join) {
      // only merge if the join segment was the last write
      // in the segment history of the join revision
      // merge the value into the master revision
      set(main, computeMerge(main, joinRev, join))
    }
  }

}

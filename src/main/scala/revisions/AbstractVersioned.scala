package revisions

import scala.collection.concurrent


abstract class AbstractVersioned[T] extends Versioned {
  protected val versions: concurrent.Map[Int, T] =
    new concurrent.TrieMap[Int, T]

  def this(initial: T) {
    this()
    set(initial)
  }

  @inline
  def value: T = get

  @inline
  def value_=(v: T): Unit = set(v)

  @inline
  def get: T =
    get(Revision.currentRevision.get())
  @inline
  def get(rev: Revision): T =
    get(rev.current)
  def get(seg: Segment): T = {
    var s = seg
    while (!versions.contains(s.version)) {
      s = s.parent
    }
    versions(s.version)
  }

  @inline
  def set(v: T): Unit =
    set(Revision.currentRevision.get(), v)
  @inline
  def set(rev: Revision, v: T): Unit =
    set(rev.current, v)
  def set(seg: Segment, v: T): Unit = {
    if (!versions.contains(seg.version)) {
      seg.written += this
    }
    versions(seg.version) = v
  }

  // release the value stored for a segment
  override def release(release: Segment): Unit =
    versions.remove(release.version)

  // collapse the value in the parent segment into the main revision
  override def collapse(main: Revision, parent: Segment): Unit = {
    if (!versions.contains(main.current.version)) {
      // if not already written in main, copy from parent
      set(main, versions(parent.version))
    }
    // release value for parent
    versions.remove(parent.version)
  }

}
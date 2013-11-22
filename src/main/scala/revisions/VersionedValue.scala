package revisions


class VersionedValue[T] extends Versioned {
  val versions: scala.collection.concurrent.Map[Int, T] =
    new scala.collection.concurrent.TrieMap[Int, T]

  def this(initial: T) {
    this()
    set(initial)
  }

  def value: T = get
  def value_=(v: T): Unit = set(v)

  def get: T = get(Revision.currentRevision.get())
  def get(r: Revision): T = {
    var s: Segment = r.current
    while (!versions.contains(s.version)) {
      s = s.parent
    }
    versions(s.version)
  }

  def set(v: T): Unit = set(Revision.currentRevision.get(), v)
  def set(r: Revision, v: T): Unit = {
    val s: Segment = r.current
    if (!versions.contains(s.version)) {
      s.written += this
    }
    versions(s.version) = v
  }

  override def release(release: Segment): Unit = versions.remove(release.version)

  override def collapse(main: Revision, parent: Segment): Unit = {
    if (!versions.contains(main.current.version)) {
      set(main, versions(parent.version))
    }
    versions.remove(parent.version)
  }

  override def merge(main: Revision, joinRev: Revision, join: Segment): Unit = {
    var s: Segment = joinRev.current
    while (!versions.contains(s.version)) {
      s = s.parent
    }
    if (s eq join) { // only merge if this was the last write
      set(main, versions(join.version))
    }
  }
}

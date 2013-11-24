package revisions


class VersionedValue[T](initial: T) extends AbstractVersioned[T](initial) {

  override def merge(main: Revision[_], joinRev: Revision[_], join: Segment): Unit = {
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
      set(main, versions(join.version))
    }
  }

}

object VersionedValue {
  def apply[T](initial: T): VersionedValue[T] =
    new VersionedValue[T](initial)
}

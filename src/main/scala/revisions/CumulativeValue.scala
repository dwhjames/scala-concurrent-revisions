package revisions


abstract class CumulativeValue[T](initial: T) extends AbstractVersioned[T](initial) {

  def mergeValue(root: T, main: T, join: T): T

  override def merge(main: Revision, joinRev: Revision, join: Segment): Unit = {
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
      set(main, mergeValue(get(joinRev.root), get(main), versions(join.version)))
    }
  }

}

object CumulativeValue {
  def apply[T](initial: T, f: (T, T, T) => T): CumulativeValue[T] =
    new CumulativeValue[T](initial) {
      override def mergeValue(root: T, main: T, join: T): T = f(root, main, join)
    }
}

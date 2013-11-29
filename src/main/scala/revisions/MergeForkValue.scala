package revisions


abstract class MergeForkValue[T](initial: T) extends AbstractVersioned[T] with ThreeWayMerge[T] {

  set(Revision.currentRevision.get(), initial)


  def forkValue(v: T): T

  private def firstWrite(rev: Revision[_]): Boolean = {
    val root = rev.root
    var seg  = rev.current
    while (seg ne root) {
      if (versions.contains(seg.version)) {
        return false
      } else {
        seg = seg.parent
      }
    }
    true
  }

  @inline
  final def modify(f: T => T): Unit =
    modify(Revision.currentRevision.get(), f)
  final def modify(rev: Revision[_], f: T => T): Unit = {
    val seg = rev.current
    val v = seg.version
    if (!versions.contains(v)) {
      seg.written += this

      if (firstWrite(rev)) {
        val x = get(rev.root)
        versions(v) = f(forkValue(x))
      } else {
        val x = get(seg)
        versions(v) = f(x)
      }
    } else {
      versions(v) = f(versions(v))
    }
    cache = ((v << 16) | (v & 0xFFFF))
  }

}

object MergeForkValue {
  def apply[T](initial: T, m: (T, T, T) => T, f: T => T): MergeForkValue[T] =
    new MergeForkValue[T](initial) {
      override def mergeValue(root: T, main: T, join: T): T = m(root, main, join)
      override def forkValue(x: T): T = f(x)
    }
}

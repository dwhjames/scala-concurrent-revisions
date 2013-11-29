package revisions


abstract class CumulativeValue[T](initial: T) extends VersionedValue[T](initial) with ThreeWayMerge[T]

object CumulativeValue {
  def apply[T](initial: T, f: (T, T, T) => T): CumulativeValue[T] =
    new CumulativeValue[T](initial) {
      override protected def mergeValue(root: T, main: T, join: T): T = f(root, main, join)
    }
}

package revisions

trait ThreeWayMerge[T] extends AbstractVersioned[T] {

  protected def mergeValue(root: T, main: T, join: T): T

  override protected def computeMerge(main: Revision[_], joinRev: Revision[_], join: Segment): T =
    mergeValue(get(joinRev.root), get(main), versions(join.version))

}

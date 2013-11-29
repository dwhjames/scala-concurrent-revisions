package revisions


class VersionedValue[T](initial: T) extends AbstractVersioned[T] {

  set(Revision.currentRevision.get(), initial)


  @inline
  final def value: T = get

  @inline
  final def value_=(v: T): Unit = set(v)

  @inline
  final def set(v: T): Unit =
    set(Revision.currentRevision.get(), v)

  override protected def computeMerge(main: Revision[_], joinRev: Revision[_], join: Segment): T =
    versions(join.version)

}

object VersionedValue {
  def apply[T](initial: T): VersionedValue[T] =
    new VersionedValue[T](initial)
}

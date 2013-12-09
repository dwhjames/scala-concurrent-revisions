
import scala.concurrent.forkjoin.{ForkJoinPool, ForkJoinTask}

package object revisions {

  def blocking[T](thunk: => T): T = {
    if (!ForkJoinTask.inForkJoinPool()) {
      thunk
    } else {
      var result: T = null.asInstanceOf[T]
      ForkJoinPool.managedBlock(new ForkJoinPool.ManagedBlocker {
        @volatile var isdone = false
        override def block(): Boolean = {
          result = try thunk finally { isdone = true }
          true
        }
        override def isReleasable = isdone
      })
      result
    }
  }

}

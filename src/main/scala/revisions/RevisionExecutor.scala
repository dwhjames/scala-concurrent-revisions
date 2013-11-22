package revisions

import java.util.concurrent.{Callable, Executors, ExecutorService, Future, ThreadFactory}
import java.util.concurrent.atomic.AtomicInteger


object RevisionExecutor {

  def global: ExecutorService = Implicits.global

  object Implicits {
    implicit lazy val global: ExecutorService = {
      val threadFactory = new ThreadFactory {
        val threadNumber = new AtomicInteger(1)
        override def newThread(r: Runnable): Thread = {
          val t = new Thread(r, s"RevisionExecutor-thread-${threadNumber.getAndIncrement()}")
          t.setDaemon(true)
          t
        }
      }
      Executors.newCachedThreadPool(threadFactory)
    }
  }
}

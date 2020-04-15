package stm.problems.zio

import zio.ZIO
import zio.console._
import zio.stm._
import zio.duration._
import zio.random
import zio.random.Random
import zio.clock.Clock

object DiningPhilosophers {
  type Fork = TSemaphore

  def makeForks(n: Int): STM[_, List[Fork]] = {
    STM.collectAll(STM.replicate(n)(TSemaphore.make(1)))
  }

  def philosophize(
      n: Int,
      queue: TQueue[String],
      forks: List[Fork]
  ): ZIO[Console with Clock with Random, _, Unit] = {
    val leftFork = forks(n)
    val rightFork = forks((n + 1) % forks.length)
    for {
      x <- random.nextInt(100)
      _ <- STM
        .atomically(
          leftFork
            .withPermit(rightFork.withPermit(queue.offer(s"Philosopher $n haz forks")))
        )
        .delay(x.millis)
    } yield ()
  }

  def action =
    for {
      forks <- STM.atomically(makeForks(5))
      queue <- STM.atomically(TQueue.unbounded[String])
      _ <- ZIO.foreach(0 until forks.length)(n =>
        philosophize(n, queue, forks).forever.fork
      )
      _ <- STM
        .atomically(queue.take)
        .flatMap(x => putStrLn(x))
        .forever
        .timeout(5.seconds)
    } yield ()
}

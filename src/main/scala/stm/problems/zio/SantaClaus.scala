package stm.problems.zio

import zio.console._
import zio.ZIO
import zio.stm._
import zio.duration._
import zio.random
import zio.random.Random
import zio.clock.Clock

object SantaClaus {
  type CountDownLatch = TSemaphore

  val elfGroupSize: Long = 3
  val elves = 7
  val reinGroupSize: Long = 10
  val reins = 10

  case class Group(
      quota: Long,
      leftQuota: TSemaphore,
      permission: TRef[TPromise[Nothing, CountDownLatch]]
  )

  def makeGroup(quota: Long): STM[Nothing, Group] =
    for {
      leftQuota <- TSemaphore.make(quota)
      promise <- TPromise.make[Nothing, CountDownLatch]
      permission <- TRef.make(promise)
    } yield Group(quota, leftQuota, permission)

  def joinGroup(group: Group): STM[_, TRef[TPromise[Nothing, CountDownLatch]]] =
    group.leftQuota.acquire *> STM.succeed(group.permission)

  def runGroup(group: Group): STM[_, CountDownLatch] = {
     for {
      latch <- makeLatch(group.quota)
      permission <- group.permission.get
      _ <- permission.succeed(latch)
      _ <- group.permission.set(permission)
    } yield latch
  }

  def clearGroup(group: Group): STM[Nothing, Unit] = {
    for {
      _ <- group.leftQuota.releaseN(group.quota)
      promise <- TPromise.make[Nothing, CountDownLatch]
      _ <- group.permission.set(promise)
    } yield ()
  }

  def makeLatch(n: Long): STM[_, CountDownLatch] = {
    TSemaphore.make(n)
  }

  def countDown(latch: CountDownLatch): STM[_, Long] = {
    latch.acquire *> latch.available
  }

  def waitForLatch(latch: CountDownLatch): STM[_, Unit] = {
    latch.available.flatMap(x => if (x == 0) STM.unit else STM.retry)
  }

  def deliverToys(n: Int, queue: TQueue[String]): STM[Nothing, Unit] = {
    queue.offer(s"Rein $n is delivering toys")
  }

  def meetInStudy(n: Int, queue: TQueue[String]): STM[Nothing, Unit] = {
    queue.offer(s"Elf $n meets Santa in the study")
  }

  def elf(
      n: Int,
      queue: TQueue[String],
      group: Group
  ): ZIO[Console with Clock with Random, _, Unit] =
    for {
      _ <- random.nextInt(100).flatMap(x => ZIO.sleep(x.millis))
      permission <- STM.atomically(joinGroup(group))
      latch <- STM.atomically(permission.get.flatMap(p => p.await))
      _ <- STM.atomically(meetInStudy(n, queue))
      _ <- STM.atomically(countDown(latch))
    } yield ()

  def rein(
      n: Int,
      queue: TQueue[String],
      group: Group
  ): ZIO[Console with Clock with Random, _, Unit] =
    for {
      _ <- random.nextInt(100).flatMap(x => ZIO.sleep(x.millis))
      permission <- STM.atomically(joinGroup(group))
      latch <- STM.atomically(permission.get.flatMap(p => {
        val r= p.await
        r
      }))
      _ <- STM.atomically(deliverToys(n, queue))
      _ <- STM.atomically(countDown(latch))
    } yield ()

  // orElse currently does not work for STM[Nothing, Unit] and STM[Nothing, Unit].
  // Also, orElse seems to be ignore the first stm once it failed or retried.
  // https://github.com/zio/zio/commit/6d41f0ae008cb9f57781ed7fbbab236c56aa6746 should have fixed it.
  def chooseGroup(
      elfGroup: Group,
      reinGroup: Group
  ): STM[_, (CountDownLatch, Group)] =
    elfGroup.leftQuota.available
      .zip(reinGroup.leftQuota.available)
      .retryUntil { case (e, r) => r == 0 || e == 0}
      .flatMap {
        case (_, r) => {
          val group =
            if (r == 0) reinGroup else elfGroup
          runGroup(group).zip(STM.succeed(group))
        }
      }

  def santa(
      elfGroup: Group,
      reinGroup: Group,
      queue: TQueue[String]
  ): ZIO[Console, _, Unit] =
    for {
      _ <- putStrLn("---------------- Almighty santa in the work")
      _ <- STM
        .atomically(chooseGroup(elfGroup, reinGroup))
        .flatMap {
          case (latch, group) => {
            STM.atomically(
              waitForLatch(latch) *> clearGroup(group)
            )
          }
        }
      _ <- STM
        .atomically(queue.takeAll)
        .flatMap(x => ZIO.foreach_(x)(x => putStrLn(x)))
    } yield ()

  def action = {
    for {
      elfGroup <- STM.atomically(makeGroup(elfGroupSize))
      reinGroup <- STM.atomically(makeGroup(reinGroupSize))
      queue <- STM.atomically(TQueue.unbounded[String])
      _ <- ZIO.foreach(1 to elves)(n => elf(n, queue, elfGroup).forever.fork)
      _ <- ZIO.foreach(1 to reins)(n => rein(n, queue, reinGroup).forever.fork)
      _ <- santa(
        elfGroup,
        reinGroup,
        queue
      ).forever.timeout(15.seconds)
    } yield ()
  }
}

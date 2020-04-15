package stm.problems.zio

import zio.console._
import zio.{ZIO, UIO}
import zio.stm._

object AtomicTransfer {
  val amount = 100000
  def transfer(
      sender: TRef[Int],
      receiver: TRef[Int],
      much: Int
  ): UIO[Unit] =
    STM.atomically {
      for {
        balance <- sender.get
        _ <- STM.check(balance >= much)
        _ <- receiver.update(_ + much)
        _ <- sender.update(_ - much)
      } yield ()
    }

  def action: ZIO[Console, _, Unit] = {
    for {
      accounts <- TRef.makeCommit(amount).zip(TRef.makeCommit(0))
      (alice, bob) = accounts
      _ <- ZIO.foreach(1 until amount)(_ => transfer(alice, bob, 1).fork)
      // _ <- ZIO.foreachParN_(1)(1 until amount)(_ => transfer(alice, bob, 1))
      // TODO: the above code throws stackoverflow exception, investigate that.
      aliceAmount <- STM.atomically(alice.get)
      bobAmount <- STM.atomically(bob.get)
      _ <- putStrLn(s"Alice has $aliceAmount in the end.")
      _ <- putStrLn(s"Bob has $bobAmount in the end.")
    } yield ()
  }
}

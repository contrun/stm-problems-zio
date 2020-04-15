package stm.problems.zio

import zio.App

object STMProblems extends App {
  def run(args: List[String]) = {
    val action = args.headOption match {
      case Some("diningphilosophers") => DiningPhilosophers.action
      case Some("atomictransfer") => AtomicTransfer.action
      case Some("santaclaus") => SantaClaus.action
      case _ => SantaClaus.action
    }
    action.foldCause(cause => {
      println(s"cause = $cause")
      1
    }, _ => 0)
  }
}


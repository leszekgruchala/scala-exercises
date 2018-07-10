package eu.gruchala.trampolines

import scala.annotation.tailrec

case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]
case class Done[+A](result: A) extends Trampoline[A]
private[this] case class FlatMap[A, +B](subroutine: Trampoline[A], continuation: A => Trampoline[B]) extends Trampoline[B]

sealed trait Trampoline[+A] { //which sounds like: wrap everything and delay execution

  @tailrec
  final def resume(): Either[() => Trampoline[A], A] = //hmm, maybe Either[More, Done]... but then less optimized?
    this match {
      case Done(result) => Right(result)
      case More(k) => Left(k)
      case FlatMap(subroutine, continuation) =>
        subroutine match {
          case Done(a) => continuation(a).resume()
          case More(sub) => Left(() => FlatMap(sub(), continuation))
          case FlatMap(sub, g) =>
            sub.flatMap((x: Any) => g(x).flatMap(continuation)).resume()
        }
    }

  @tailrec
  final def runT: A =
    resume() match {
      case Left(k) => k().runT
      case Right(result) => result
    }

  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
    this match {
      case FlatMap(subroutine, continuation) =>
        FlatMap(subroutine, (x: Any) => continuation(x).flatMap(f))
      case x => FlatMap(x, f)
    }

  def map[B](f: A => B): Trampoline[B] =
    flatMap(a => Done(f(a)))
}

case class TState[S, +A](runS: S => Trampoline[(A, S)]) {

  def map[B](f: A => B): TState[S, B] = {
    TState[S, B](s => {
      val (a, s1) = runS(s).runT
      Done(f(a) -> s1)
    })
  }

  def flatMapWithStackOverflow[B](f: A => TState[S, B]): TState[S, B] = {
    TState[S, B](s => {
        val (a, s1) = runS(s).runT
        More(() => f(a).runS(s1))
      }
    )
  }

  def flatMap[B](f: A => TState[S, B]): TState[S, B] =
    TState[S, B](s => runS(s).flatMap { //Needed `More(() => runS(s).flatMap`?
      case (a, s1) => More(() => f(a).runS(s1))
    })

}

object TStateOps {

  def getTState[S]: TState[S, S] =
    TState(s => Done(s -> s))

  def setTState[S](s: S): TState[S, Unit] =
    TState(_ => Done(() -> s))

  def pureTState[S, A](a: A): TState[S, A] =
    TState(s => Done(a -> s))

  def zipIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(pureTState[Int, List[(Int, A)]](List.empty))((accTState, a) => for {
      zipped <- accTState
      index <- getTState
      _ <- setTState(index + 1)
    } yield (index, a) :: zipped).runS(0).runT._1.reverse
}

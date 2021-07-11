trait IO[-R, E, +A]:
  def map[B](f: A => B): IO[R, E, B] = ???

  def flatMap[R2, E2, B](f: A => IO[R2, E2, B])(implicit ev: E => E | E2): IO[R2 & R, E | E2, B] = ???

  def foldM[R2, E2, A2, R3, E3, A3](f: A => IO[R2, E2, A2], g: E => IO[R3, E3, A3]): IO[R3 & R2 & R, E3 | E2, A3 | A2] = ???

  def inject[R2, R3](r: R2)(implicit tag: Tag[R2], ev: R2 & R3 => R): IO[R3, E, A] = ???

  def catchAll[R2, E2, A2](f: E => IO[R2, E2, A2]) = ???
  
  def catchSome[R2, E2, A2, E3 <: E, E4 <: E](f: PartialFunction[E, (E3, IO[R2, E2, A2])])(implicit ev: E => E3 | E4): IO[R & R2, E2 | E4, A | A2] = ???

class Tag[A]()

enum Exit[+E, +A]:
  case Fail(e: E)
  case Succeed(a: A)

enum ContiuationFrame:
  case FlatMap(val f: Any => IO[Any, Any, Any])
  case Fold(val f: Any => IO[Any, Any, Any], val g: Any => IO[Any, Any, Any])

extension [E, A](zio: IO[Any, E, A])
  def unsafeRun: Exit[E, A] = ???

object IO {
  def succeed[A](a: => A): IO[Any, Nothing, A] = ???

  def fail[E](e: => E): IO[Any, E, Nothing] = ???

  def use[R, R2, E2, A](f: R => IO[R2, E2, A])(implicit tag: Tag[R]): IO[R & R2, E2, A] = ???
}
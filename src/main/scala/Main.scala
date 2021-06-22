sealed trait IO[R, E, A]

class Tag[A]()
class Has[A]()

enum Exit[E, A]:
  case Fail(e: E)
  case Succeed(a: A)

extension [R, E, A](io: IO[R, E, A])
  def map[B](f: A => B): IO[R, E, B] = ???
  def flatMap[R2, E2, B](f: A => IO[R2, E2, B]): IO[R2 & R, E2 | E, B] = ???
  def foldM[R2, E2, A2, R3, E3, A3](f: A => IO[R2, E2, A2], g: E => IO[R3, E3, A3]): IO[R3 & R2 & R, E3 | E2, A3 | A2] = ???
  def inject[R2, R3](r: R2)(implicit tag: Tag[R2], ev: Has[R2] & R3 => R): IO[R3, E, A] = ???
  def catchAll[R2, E2, A2](f: E => IO[R2, E2, A2]) = ???  
  def catchSome[R2, E2, A2, E3, E4](f: PartialFunction[E, (E3, IO[R2, E2, A2])])(implicit ev: E => E3 | E4): IO[R & R2, E2 | E4, A | A2] = ???

object IO {
  def succeed[A](a: => A): IO[Any, Nothing, A] = ???
  def fail[E](e: => E): IO[Any, E, Nothing] = ???
  def use[R, R2, E2, A](f: R => IO[R2, E2, A])(implicit tag: Tag[R]): IO[Has[R] & R2, E2, A] = ???
}

trait Console {
  def log(msg: String): IO[Any, Nothing, Unit]
}

object Console {
  implicit val tag: Tag[Console] = Tag()
}

trait Math {
  def add(x: Int, y: Int): IO[Any, Nothing, Int]
}

object Math {
  implicit val tag: Tag[Math] = Tag()

  val Live: Math = ???
}

val program = for {
  x <- IO.succeed(1)
  y <- IO.succeed(2)
  z <- IO.use((T: Math) => T.add(x, y))
  _ <- IO.use((T: Console) => T.log(s"z: ${z}"))
} yield ()

// BUGGED AS IO[Nothing, Nothing, Unit]
val main = program inject Math.Live
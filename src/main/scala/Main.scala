sealed trait IO[R, E, A]

object IO {
  def of[R, E, A](): IO[R, E, A] = ???
}

extension [R, E, A](x: IO[R, E, A])
  def map[B](f: A => B): IO[R, E, B] = ???
  def flatMap[R2, E2, B](f: A => IO[R2, E2, B]): IO[R & R2, E | E2, B] = ???

case class EnvA()
case class ErrA()

case class EnvB()
case class ErrB()

val a: IO[EnvA, ErrA, Int] = ???
val f: Int => IO[EnvB, ErrB, Int] = ???

// ErrB shows twice
// IO[EnvA & EnvB, ErrA | ErrB | ErrB, Int] 
val b = IO.of[EnvA, ErrA, Int]()
  .flatMap(x => IO.of[EnvB, ErrB, Int]())
  .flatMap(x => IO.of[EnvB, ErrB, Int]())

// OK
// IO[EnvA & EnvB, ErrA | ErrB, Int]
val c = for {
  x <- a
  y <- f(x)
  z <- f(y)
} yield z

// VSCODE See this as IO[EnvA & EnvB, ErrA | (ErrB | (ErrB | E2)), Int]
// sbt console raise an error
val d = for {
  x <- a
  y <- f(x)
  z <- f(y)
  _ <- IO.of[Any, Nothing, Unit]()
} yield z
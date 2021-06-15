//
// USAGE
//

trait Console {
  def putStrLn(msg: => String): IO[Any, Nothing, Unit]
}

object Console {
  implicit val tag: Tag[Console] = Tag()

  def putStrLn(msg: => String) = IO use ((c: Console) => c putStrLn msg)
}

trait Math {
  def add(x: Int, y: Int): IO[Any, Nothing, Int]
}

object Math {
  implicit val tag: Tag[Math] = Tag()

  def add(x: Int, y: Int) = IO use ((c: Math) => c add(x, y))
}

case class ErrorA(a: String)
case class ErrorB(b: String)

val program = for {
  _ <- Console putStrLn "hello"
  _ <- Console putStrLn "world"
  y <- Math add(2, 3)
  _ <- Console putStrLn s"result: $y"
} yield ()

val main = program
  .inject(new Console {
    def putStrLn(msg: => String) = IO.succeed(println(msg))
  })
  .inject(new Math {
    def add(x: Int, y: Int) = IO.succeed(x + y)
  })

@main def root() = {
  main.unsafeRun match {
    case Exit.Fail(e) => {
      println("Error:")
      println(e)
      ()
    }
    case Exit.Succeed(a) => ()
  }
}
//
// USAGE
//

trait Random {
  def int: IO[Any, Nothing, Int]
}

object Random {
  given Tag[Random] = Tag()

  def int = IO use ((T: Random) => T.int)
}

trait Console {
  def putStrLn(msg: => String): IO[Any, Nothing, Unit]
}

object Console {
  given Tag[Console] = Tag()

  def putStrLn(msg: => String) = IO use ((T: Console) => T putStrLn msg)
}

trait Math {
  def add(x: Int, y: Int): IO[Any, Nothing, Int]
}

object Math {
  given Tag[Math] = Tag()

  def add(x: Int, y: Int) = IO use ((T: Math) => T add(x, y))
}

case class ErrorA(a: String)
case class ErrorB(b: String)
case class ErrorC(c: String)

val program = for {
  _ <- Console putStrLn "hello"
  _ <- Console putStrLn "world"
  y <- Math add(2, 3)
  _ <- Random.int
  _ <- Console putStrLn s"result: $y"
  _ <- IO fail ErrorA("a")
  _ <- IO fail ErrorB("b")
  _ <- IO fail ErrorC("c")
} yield ()

val main = program
  .catchSome({
    case x: ErrorA => (x, Console putStrLn "recovered from A")
  })
  .catchSome({
    case x: ErrorB => (x, IO fail ErrorC("c"))
  })
  .inject(new Console {
    def putStrLn(msg: => String) = IO.succeed(println(msg))
  })
  .inject(new Math {
    def add(x: Int, y: Int) = IO.succeed(x + y)
  })
  .inject(new Random {
    def int = IO.succeed(1)
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
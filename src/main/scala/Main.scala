//
// USAGE
//

trait Random {
  def int: IO[Any, Nothing, Int]
}

object Random {
  given Tag[Random] = Tag()

  val Live = Layer(new Random {
    def int = IO.succeed(1)
  })

  def int = IO use ((T: Random) => T.int)
}

trait Layout {
  def prefix: IO[Any, Nothing, String]
}

object Layout {
  given Tag[Layout] = Tag()

  val Live = Layer(new Layout {
    def prefix = IO succeed "prefix: "
  })

  def prefix = IO use ((T: Layout) => T.prefix)
}

trait Console {
  def putStrLn(msg: => String): IO[Any, Nothing, Unit]
}

object Console {
  given Tag[Console] = Tag()

  val Live = Layer(for {
    prefix <- Layout.prefix
  } yield new Console {
    def putStrLn(msg: => String) = IO.succeed(println(s"${prefix}${msg}"))
  })

  def putStrLn(msg: => String) = IO use ((T: Console) => T putStrLn msg)
}

trait Math {
  def add(x: Int, y: Int): IO[Any, Nothing, Int]
}

object Math {
  given Tag[Math] = Tag()

  val Live = Layer(new Math {
    def add(x: Int, y: Int) = IO.succeed(x + y)
  })

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
  .inject(Console.Live +++ Math.Live +++ Random.Live)
  .inject(Layout.Live)

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
import org.junit.Test
import org.junit.Assert.*

class IOSuite:
  @Test def add(): Unit = {
    val program = Math.add(2, 1) inject new Math {
      def add(x: Int, y: Int) = IO.succeed(x + y)
    }
    assertEquals(program.unsafeRun, Exit.Succeed(3))
  }

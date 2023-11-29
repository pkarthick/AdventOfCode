import scala.compiletime.ops.string
@main def main =
  println("Hello from Scala!")

  class X {

    def inner() = "Hello "

    def inner2[A](a: A) = a

  }

  println(X().inner2("Testing:\n"))
  println(X().inner() + X().inner2(" world!!!"));

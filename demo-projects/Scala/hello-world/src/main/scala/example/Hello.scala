package example

object Hello extends Greeting with App {
  println(greeting)
  println(greeting)
}

trait Greeting {
  lazy val greeting: String = "hello"
}

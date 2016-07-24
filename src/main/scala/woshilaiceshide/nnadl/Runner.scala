package woshilaiceshide.nnadl

import woshilaiceshide.nnadl.util.Utility
import woshilaiceshide.nnadl.math._

object Runner extends App {

  val network = new Network(Array(1, 2, 3))
  println(network)

  import Network._
  val a = Matrix.vertical(Array(1.0d, 2.0d, 3.0d))
  val b = Matrix.horizontal(Array(4.0d, 5.0d, 6.0d))

  println(b.dot(a))

}
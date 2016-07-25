package woshilaiceshide.nnadl

import woshilaiceshide.nnadl.util.Utility
import woshilaiceshide.nnadl.math._

object Runner extends App {

  import Network._

  val network = new Network(Array(784, 30, 10))
  //println(network)

  val a = Matrix.vertical(Array(1.0d, 2.0d, 3.0d))
  val b = Matrix.horizontal(Array(4.0d, 5.0d, 6.0d))
  println(b.dot(a))

  //println(MnistLoader.load_data())
  println(MnistLoader.vectorized_result(3))
  println(MnistLoader.default_folder)

  val rnd: scala.util.Random = new scala.util.Random()

}
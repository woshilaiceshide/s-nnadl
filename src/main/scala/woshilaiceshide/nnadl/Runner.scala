package woshilaiceshide.nnadl

import woshilaiceshide.nnadl.math._
import woshilaiceshide.nnadl.util._

import woshilaiceshide.nnadl.mnist._

object Runner extends App {

  def test_matrix() = {
    val rnd: scala.util.Random = new scala.util.Random()
    val a = Matrix.vertical(Array(1.0d, 2.0d, 3.0d)).transpose()
    val b = Matrix.horizontal(Array(4.0d, 5.0d, 6.0d)).transpose()
    println(b.dot(a))
  }
  //test_matrix()

  def test_mnist() = {
    //println(MnistLoader.load_data())
    //println(MnistLoader.vectorized_result(3))
    //println(MnistLoader.default_folder)
    val loaded = MnistLoader.load_data_wrapper()
    loaded.test_data.map { _.save() }
  }
  //test_mnist()

  def test_network() = {

    //println(new Network(Array(784, 30, 10)))
    //val network = new Network(Array(784, 30, 10))
    val network = new MultiThreadingNetwork(Array(784, 30, 10))
    val MnistDataSet(training_data, validation_data, test_data) = MnistLoader.load_data_wrapper()
    network.SGD(training_data, 1000, 400, 3.0d, test_data = Some(test_data), 4)
  }
  test_network()

}
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
    val loaded = MnistLoader.load_data_wrapper(1)
    loaded.test_data.map { x => MnistRecord(x).save() }
  }
  //test_mnist()

  def test_network() = {

    //println(new Network(Array(784, 30, 10)))
    //val network = new Network(Array(784, 30, 10))
    //the most rate is 96.78%
    val network = new MultiThreadingNetwork(Array(784, 30, 10))
    val MnistDataSet(training_data, validation_data, test_data) = MnistLoader.load_data_wrapper(10)
    network.SGD(training_data, 1000, 40, 8.0d, test_data = Some(test_data), 1)
  }
  //test_network()

  def test_configurable_network() = {

    //println(new Network(Array(784, 30, 10)))
    //val network = new Network(Array(784, 30, 10))
    //the most rate is 96.78%
    val MnistDataSet(training_data, validation_data, test_data) = MnistLoader.load_data_wrapper(10)
    val network = new ConfigurableNetwork(Array(784, 30, 10), ConfigurableNetwork.Configurator(lambda = 1))
    network.SGD(training_data.take(training_data.length), 1000, 10, 1.0d, test_data = Some(test_data))
  }
  test_configurable_network()

  def test_addhoc() = {

    import ArrayUtility._
    //println(Calc.sigmoid(1 * 2 + 2))
    println(Calc.softmax(Array(1d, 2d, 3d)).map { _.toString }.mkString(", "))
    println(Calc.softmax(Array(1d, 2d, 3d), 100d).map { _.toString }.mkString(", "))

    val a = Array(1, 2, 3)
    def add1(i: Int) = {

      println(i)
      i + 1
    }
    val b = a.map { x => add1(x) }

    val t = Matrix.random(new scala.util.Random(), 2, 2)
    println(t)
    println(t * 1.0d)

  }
  test_addhoc()

}
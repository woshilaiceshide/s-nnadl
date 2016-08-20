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
    val loaded = MnistLoader.load_data_wrapper(1)
    loaded.test_data.map { x => MnistRecord(x).save() }
  }
  //test_mnist()

  def test_network() = {
    val network = new MultiThreadingNetwork(Array(784, 30, 10), ConfigurableNetwork.Configurator(dropout_proportion = Some(0.04d), regularization = new ConfigurableNetwork.L2Regularization(lambda = 1)))
    val MnistDataSet(training_data, validation_data, test_data) = MnistLoader.load_data_wrapper(1)
    val expanded_training_data = training_data ++
      training_data.map { r => NRecord(r.x.reshape(28, 28).shift_row(+1).reshape(784, 1), r.y) } ++
      training_data.map { r => NRecord(r.x.reshape(28, 28).shift_row(-1).reshape(784, 1), r.y) } ++
      training_data.map { r => NRecord(r.x.reshape(28, 28).shift_row(+2).reshape(784, 1), r.y) } ++
      training_data.map { r => NRecord(r.x.reshape(28, 28).shift_row(-2).reshape(784, 1), r.y) } ++
      training_data.map { r => NRecord(r.x.reshape(28, 28).shift_column(+1).reshape(784, 1), r.y) } ++
      training_data.map { r => NRecord(r.x.reshape(28, 28).shift_column(-1).reshape(784, 1), r.y) } ++
      training_data.map { r => NRecord(r.x.reshape(28, 28).shift_column(+2).reshape(784, 1), r.y) } ++
      training_data.map { r => NRecord(r.x.reshape(28, 28).shift_column(-2).reshape(784, 1), r.y) }

    network.SGD(expanded_training_data, 8000, 100, 3d, test_data = Some(test_data), 4)
  }
  test_network()

  def test_configurable_network() = {
    val MnistDataSet(training_data, validation_data, test_data) = MnistLoader.load_data_wrapper(1)
    val network = new ConfigurableNetwork(Array(784, 30, 10), ConfigurableNetwork.Configurator(dropout_proportion = Some(0.3d), regularization = new ConfigurableNetwork.L2Regularization(lambda = 1)))
    network.SGD(training_data.take(training_data.length), 1000, 10, 1.0d, test_data = Some(test_data))
  }
  //test_configurable_network()

  def test_addhoc() = {

    import ArrayUtility._
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

    println(Math.signum(6.31123d))
  }
  //test_addhoc()

}
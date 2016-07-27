package woshilaiceshide.nnadl

import woshilaiceshide.nnadl.math._
import woshilaiceshide.nnadl.util._

object Runner extends App {

  import MnistLoader._

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
    val MnistDataSet(training_data, validation_data, test_data) = TextMnistLoader.load_data_wrapper()
    network.SGD(training_data, 1000, 500, 3.0d, test_data = Some(test_data), 4)
  }
  test_network()

  def test_pickle() = {
    import java.io._
    import net.razorvine.pickle.Unpickler

    import net.razorvine.pickle.IObjectConstructor

    val c = new IObjectConstructor() {
      def construct(args: Array[Object]): Object = {
        List[Object]()
      }
    }

    val o = new IObjectConstructor() {
      def construct(args: Array[Object]): Object = {
        new Object()
      }
    }

    val stream = new FileInputStream("""/opt/workspace/s-nnadl/data/mnist.pkl.gz""")
    val gzip = new java.util.zip.GZIPInputStream(stream)
    val unpickler = new Unpickler()
    Unpickler.registerConstructor("numpy.core.multiarray", "_reconstruct", c)
    Unpickler.registerConstructor("numpy", "dtype", o)
    val data = unpickler.load(gzip)
  }

  //test_pickle()

}
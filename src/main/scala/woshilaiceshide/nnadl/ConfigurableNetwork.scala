package woshilaiceshide.nnadl

import woshilaiceshide.nnadl.util._
import woshilaiceshide.nnadl.util.Utility._
import woshilaiceshide.nnadl.util.ArrayUtility._
import woshilaiceshide.nnadl.math._

import woshilaiceshide.nnadl.mnist._

object ConfigurableNetwork {

  trait WeightsInitializer {
    def initialize_weights(rnd: scala.util.Random, r_count: Int, c_count: Int): Matrix
  }

  object LargeWeightsInitializer extends WeightsInitializer {
    def initialize_weights(rnd: scala.util.Random, r_count: Int, c_count: Int) = Matrix.random(rnd, r_count, c_count)
  }

  object DefaultWeightsInitializer extends WeightsInitializer {
    def initialize_weights(rnd: scala.util.Random, r_count: Int, c_count: Int) = Matrix.random(rnd, r_count, c_count, Math.sqrt(r_count))
  }

  case class Configurator(
      rnd: scala.util.Random = new scala.util.Random(),
      weights_initializer: WeightsInitializer,
      cost_function: CostFunction) {

    def initialize_weights(r_count: Int, c_count: Int) = weights_initializer.initialize_weights(rnd, r_count, c_count)

  }

}

import ConfigurableNetwork._

class ConfigurableNetwork(sizes: Array[Int], configurator: Configurator) {

  protected implicit val CLASSTAG_DOUBLE = scala.reflect.ClassTag.Double
  protected implicit val CLASSTAG_MATRIX = scala.reflect.ClassTag(classOf[Matrix])

  assert(sizes.length >= 2)

  override def toString() = {
    val ident = "   "
    val formatted_biases = {
      biases.length.range.map { i =>
        s"""${ident}layer ${i + 1}:
${biases(i).format(ident + ident)}"""
      }
    }

    val formatted_weights = {
      weights.length.range.map { i =>
        s"""${ident}layer ${i + 1}:
${weights(i).format(ident + ident)}"""
      }
    }

    s"""layers count: ${num_layers}
layers sizes: ${sizes.mkString(", ")}
current biases: 
${formatted_biases.mkString(System.lineSeparator())}
current weights:
${formatted_weights.mkString(System.lineSeparator())}"""
  }

  val num_layers = sizes.length

  val biases = {
    val tmp = new Array[Matrix](sizes.length - 1)
    var i = 1
    while (i < sizes.length) {
      tmp(i - 1) = configurator.initialize_weights(sizes(i), 1)
      i = i + 1
    }
    tmp
  }

  val weights = {
    val tmp = new Array[Matrix](sizes.length - 1)
    var i = 1
    while (i < sizes.length) {
      tmp(i - 1) = configurator.initialize_weights(sizes(i), sizes(i - 1))
      i = i + 1
    }
    tmp
  }

  def feedforward(input: Array[Double]): Matrix = feedforward(Matrix.vertical(input))

  def feedforward(input: Matrix): Matrix = {
    var a = input
    var i = 0
    while (i < biases.length) {
      val b = biases(i); val w = weights(i)
      a = Calc.sigmoid(w.dot(a) + b)
      i = i + 1
    }
    a
  }

  def SGD(
    training_data: Array[MnistRecord1],
    epochs: Int,
    mini_batch_size: Int,
    eta: Double,
    test_data: Option[Array[MnistRecord2]]): Unit = {
    val n = training_data.length
    epochs.range.map { j =>

      val start = System.currentTimeMillis()

      val shuffled = training_data.shuffle_directly(configurator.rnd)
      val mini_batches = shuffled.grouped_with_fixed_size(mini_batch_size)
      mini_batches.map { mini_batch => update_mini_batch(mini_batch, eta) }

      test_data match {
        case Some(test_data) if j % 5 == 0 =>
          val end = System.currentTimeMillis()
          println(s"""Epoch ${j}: ${end - start}ms ${evaluate(test_data)} / ${test_data.length}""")
        case _ =>
          val end = System.currentTimeMillis()
          println(s"""Epoch ${j}: ${end - start}ms completed""")
      }
    }
  }

  protected def zeros_with_the_same_shape(a: Array[Matrix]) = {
    val tmp = new Array[Matrix](a.length)
    var i = 0
    while (i < tmp.length) {
      tmp(i) = a(i).zeros_with_the_same_shape()
      i = i + 1
    }
    tmp
  }

  def update_mini_batch(mini_batch: Array[MnistRecord1], eta: Double) = {

    val nabla_b = zeros_with_the_same_shape(biases)
    val nabla_w = zeros_with_the_same_shape(weights)

    def collect_backprops() {
      var i = 0
      while (i < mini_batch.length) {
        val x = mini_batch(i)
        val (delta_nabla_b, delta_nabla_w) = backprop(x.image, x.label)

        {
          var j = 0
          while (j < delta_nabla_b.length) {
            nabla_b(j) = nabla_b(j) + delta_nabla_b(j)
            j = j + 1
          }
        }

        {
          var j = 0
          while (j < delta_nabla_w.length) {
            nabla_w(j) = nabla_w(j) + delta_nabla_w(j)
            j = j + 1
          }
        }

        i = i + 1
      }
    }
    collect_backprops()

    def learn_from_backprops() {
      val learned_layers = sizes.length - 1
      var i = 0
      while (i < learned_layers) {
        weights(i).substract_directly(nabla_w(i), (eta / mini_batch.length))
        biases(i).substract_directly(nabla_b(i), (eta / mini_batch.length))
        i = i + 1
      }
    }

    learn_from_backprops()

  }

  def backprop(x: Matrix, y: Matrix) = {

    val nabla_b = zeros_with_the_same_shape(biases)
    val nabla_w = zeros_with_the_same_shape(weights)

    val activations = new Array[Matrix](num_layers); activations(0) = x
    val zs = new Array[Matrix](num_layers - 1)

    def feedforward_with_details() = {
      var i = 0
      while (i < biases.length) {
        val z = weights(i).dot(activations(i)) + biases(i)
        zs(i) = z
        activations(i + 1) = Calc.sigmoid(z)
        i = i + 1
      }
    }
    feedforward_with_details()

    val delta = configurator.cost_function.delta(zs.t(0), activations.t(0), y)
    nabla_b.set_t(0, delta)
    nabla_w.set_t(0, delta.dot(activations.t(-1).transpose()))

    def back() = {
      var i = 1
      while (i < num_layers - 1) {
        val z = zs.t(-i)
        val sp = Calc.sigmoid_prime(z)
        val delta = weights.t(-i + 1).transpose().dot(nabla_b.t(-i + 1)) * sp

        nabla_b.set_t(-i, delta)
        nabla_w.set_t(-i, delta.dot(activations.t(-i - 1).transpose()))

        i = i + 1
      }
    }
    back()

    (nabla_b, nabla_w)
  }

  def cost_derivative(output_activations: Matrix, y: Matrix) = { output_activations - y }

  def evaluate(test_data: Array[MnistRecord2]) = {
    var corrected = 0
    var i = 0
    while (i < test_data.length) {
      val record = test_data(i)
      if (Matrix.argmax(feedforward(record.image), 0)(0) == record.label)
        corrected = corrected + 1
      i = i + 1
    }
    corrected
  }

}
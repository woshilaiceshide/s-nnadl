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

  trait ActivationFunction {
    def activate(z: Matrix): Matrix
    def prime(z: Matrix): Matrix
  }

  object SigmoidActivationFunction extends ActivationFunction {
    def activate(z: Matrix): Matrix = Calc.sigmoid(z)
    def prime(z: Matrix): Matrix = Calc.sigmoid_prime(z)
  }

  object TanhActivationFunction extends ActivationFunction {
    def activate(z: Matrix): Matrix = Calc.tanh(z)
    def prime(z: Matrix): Matrix = Calc.tanh_prime(z)
  }

  object LinearActivationFunction extends ActivationFunction {
    def activate(z: Matrix): Matrix = z
    def prime(z: Matrix): Matrix = z.map(new Matrix.ValueTransformer { def apply(v: Double): Double = 1.0d })
  }

  object SoftmaxActivationFunction extends ActivationFunction {
    def activate(z: Matrix): Matrix = Calc.softmax(z, 1)
    def prime(z: Matrix): Matrix = z.map(new Matrix.ValueTransformer { def apply(v: Double): Double = 1.0d })
  }

  trait CostFunction {
    //1. positive; 2. smaller if 'a' is closer to 'y', zero if 'a' is equal to 'y'
    def calc(a: Matrix, y: Matrix): Matrix
    //provides ∂C/∂z
    def delta(z: Matrix, a: Matrix, y: Matrix): Matrix
    //the activation function used on the output layer
    def final_activation_function: ActivationFunction
  }

  trait Regularization {
    def calc(weights: Array[Matrix], n: Int): Double
    def prime(w: Matrix, n: Int): Matrix
  }

  class L2Regularization(val lambda: Double) extends Regularization {

    def calc(weights: Array[Matrix], n: Int): Double = {
      val sum = weights.map { m =>
        var sum = 0.0d
        var i = 0
        while (i < m.r_count) {
          var j = 0
          while (j < m.c_count) {
            sum = sum + Math.pow(m(i)(j), 2)
            j = j + 1
          }
          i = i + 1
        }
        sum
      }.sum
      (lambda / n) * (sum / 2)
    }
    def prime(w: Matrix, n: Int): Matrix = {
      w * (lambda / n)
    }

  }
  class L1Regularization(val lambda: Double) extends Regularization {

    def calc(weights: Array[Matrix], n: Int): Double = {
      val sum = weights.map { m =>
        var sum = 0.0d
        var i = 0
        while (i < m.r_count) {
          var j = 0
          while (j < m.c_count) {
            sum = sum + Math.abs(m(i)(j))
            j = j + 1
          }
          i = i + 1
        }
        sum
      }.sum
      (lambda / n) * sum
    }
    def prime(w: Matrix, n: Int): Matrix = {
      Calc.sign(w, lambda / n)
    }

  }

  case class Configurator(
      rnd: scala.util.Random = new scala.util.Random(),
      weights_initializer: WeightsInitializer = DefaultWeightsInitializer,
      cost_function: CostFunction = new CrossEntropyCostFunction(SigmoidActivationFunction),
      activation_function: ActivationFunction = SigmoidActivationFunction,
      regularization: Regularization) {

    def initialize_weights(r_count: Int, c_count: Int) = weights_initializer.initialize_weights(rnd, r_count, c_count)

  }

}

import ConfigurableNetwork._

class ConfigurableNetwork(sizes: Array[Int], val configurator: Configurator) {

  import configurator._

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
      tmp(i - 1) = initialize_weights(sizes(i), 1)
      i = i + 1
    }
    tmp
  }

  val weights = {
    val tmp = new Array[Matrix](sizes.length - 1)
    var i = 1
    while (i < sizes.length) {
      tmp(i - 1) = initialize_weights(sizes(i), sizes(i - 1))
      i = i + 1
    }
    tmp
  }

  def feedforward(input: Array[Double]): Matrix = feedforward(Matrix.vertical(input))

  def feedforward(input: Matrix): Matrix = {
    var a = input
    var i = 0
    while (i < biases.length - 1) {
      val b = biases(i); val w = weights(i)
      a = activation_function.activate(w.dot(a) + b)
      i = i + 1
    }

    val b = biases(i); val w = weights(i)
    a = cost_function.final_activation_function.activate(w.dot(a) + b)

    a
  }

  def SGD(
    training_data: Array[NRecord],
    epochs: Int,
    mini_batch_size: Int,
    eta: Double,
    test_data: Option[Array[NRecord]]): Unit = {
    val n = training_data.length
    epochs.range.map { j =>

      val start = System.currentTimeMillis()

      val shuffled = training_data.shuffle_directly(rnd)
      val mini_batches = shuffled.grouped_with_fixed_size(mini_batch_size)
      mini_batches.map { mini_batch => update_mini_batch(mini_batch, eta, training_data.length) }

      test_data match {
        case Some(test_data) if j % 1 == 0 =>
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

  protected def update_mini_batch(mini_batch: Array[NRecord], eta: Double, n: Int) = {

    val nabla_b = zeros_with_the_same_shape(biases)
    val nabla_w = zeros_with_the_same_shape(weights)

    def collect_backprops() {
      var i = 0
      while (i < mini_batch.length) {
        val x = mini_batch(i)
        val (delta_nabla_b, delta_nabla_w) = backprop(x.x, x.y)

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
        //weights(i).substract_directly(nabla_w(i), (eta / mini_batch.length))
        weights(i).substract_directly(regularization.prime(weights(i), n), eta).substract_directly(nabla_w(i), (eta / mini_batch.length))
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
      while (i < biases.length - 1) {
        val z = weights(i).dot(activations(i)) + biases(i)
        zs(i) = z
        activations(i + 1) = activation_function.activate(z)
        i = i + 1
      }

      val z = weights(i).dot(activations(i)) + biases(i)
      zs(i) = z
      activations(i + 1) = cost_function.final_activation_function.activate(z)
    }
    feedforward_with_details()

    val delta = cost_function.delta(zs.t(0), activations.t(0), y)
    nabla_b.set_t(0, delta)
    nabla_w.set_t(0, delta.dot(activations.t(-1).transpose()))

    def back() = {
      var i = 1
      while (i < num_layers - 1) {
        val z = zs.t(-i)
        val zp = activation_function.prime(z)
        val delta = weights.t(-i + 1).transpose().dot(nabla_b.t(-i + 1)) * zp

        nabla_b.set_t(-i, delta)
        nabla_w.set_t(-i, delta.dot(activations.t(-i - 1).transpose()))

        i = i + 1
      }
    }
    back()

    (nabla_b, nabla_w)
  }

  def total_cost(data: Array[NRecord]) = {

    var cost = 0.0d
    var i = 0
    while (i < data.length) {
      val dn = data(i)
      val a = feedforward(dn.x)
      cost = cost + cost_function.calc(a, dn.y).row(0).sum / data.length
      i = i + 1
    }

    val r = regularization.calc(weights, data.length)
    cost + r

  }

  def evaluate(test_data: Array[NRecord]) = {
    var corrected = 0
    var i = 0
    while (i < test_data.length) {
      val record = test_data(i)
      val digital = Matrix.argmax(feedforward(record.x), 0)(0)
      if (1 == record.y(digital)(0))
        corrected = corrected + 1
      i = i + 1
    }
    corrected
  }

}
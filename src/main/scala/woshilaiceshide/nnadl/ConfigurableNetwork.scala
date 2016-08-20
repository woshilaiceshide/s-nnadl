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
    def prime(weight: Matrix, n: Int): Matrix
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
    def prime(weight: Matrix, n: Int): Matrix = {
      weight * (lambda / n)
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
    def prime(weight: Matrix, n: Int): Matrix = {
      Calc.sign(weight, lambda / n)
    }

  }

  case class Configurator(
      rnd: scala.util.Random = new scala.util.Random(),
      weights_initializer: WeightsInitializer = DefaultWeightsInitializer,
      cost_function: CostFunction = new CrossEntropyCostFunction(SigmoidActivationFunction),
      activation_function: ActivationFunction = SigmoidActivationFunction,
      regularization: Regularization,
      dropout_proportion: Option[Double] = None) {

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

  protected var biases = {
    val tmp = new Array[Matrix](sizes.length - 1)
    var i = 1
    while (i < sizes.length) {
      tmp(i - 1) = initialize_weights(sizes(i), 1)
      i = i + 1
    }
    tmp
  }

  protected var weights = {
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

  private var weights_without_dropout: Array[Matrix] = _
  private var biases_without_dropout: Array[Matrix] = _
  private var dropout_filters: Array[Array[Int]] = _

  protected def dropout(proportion: Double, rnd: scala.util.Random): Unit = {

    assert(weights.length > 1)

    val filters = new Array[Array[Int]](weights.length - 1)

    {
      var i = 0
      while (i < weights.length - 1) {

        val a = weights(i)
        val dropped_count = (a.r_count * proportion).toInt
        val filter = a.r_count.range.toArray.shuffle_directly(rnd).drop(dropped_count).sorted
        filters(i) = filter
        i = i + 1
      }
    }

    val weights1 = new Array[Matrix](weights.length)
    val biases1 = new Array[Matrix](biases.length)

    {
      var i = 0

      weights1(i) = weights(i).drop_row(filters(i))
      biases1(i) = biases(i).drop_row(filters(i))
      i = i + 1

      while (i < weights.length - 1) {
        weights1(i) = weights(i).drop(filters(i), filters(i - 1))
        biases1(i) = biases(i).drop_row(filters(i))
        i = i + 1
      }

      weights1(i) = weights(i).drop_column(filters(i - 1))
      biases1(i) = biases(i)

    }

    weights_without_dropout = weights
    biases_without_dropout = biases

    weights = weights1
    biases = biases1

    dropout_filters = filters

  }

  protected def merge_dropout(): Unit = {

    assert(weights.length > 1)
    assert(dropout_filters != null)

    {
      var i = 0

      weights(i) = weights_without_dropout(i).merge_row(weights(i), dropout_filters(i))
      biases(i) = biases_without_dropout(i).merge_row(biases(i), dropout_filters(i))
      i = i + 1

      while (i < weights.length - 1) {
        weights(i) = weights_without_dropout(i).merge(weights(i), dropout_filters(i), dropout_filters(i - 1))
        biases(i) = biases_without_dropout(i).merge_row(biases(i), dropout_filters(i))
        i = i + 1
      }

      weights(i) = weights_without_dropout(i).merge_column(weights(i), dropout_filters(i - 1))
      biases(i) = biases_without_dropout(i)
    }

    weights_without_dropout = null
    biases_without_dropout = null
    dropout_filters = null
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
      mini_batches.map { mini_batch =>
        dropout_proportion match {
          case Some(dp) => {
            if (weights.length > 1) dropout(dp, rnd)
            update_mini_batch(mini_batch, eta, training_data.length)
            if (weights.length > 1) merge_dropout()
          }
          case None => update_mini_batch(mini_batch, eta, training_data.length)
        }
      }

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
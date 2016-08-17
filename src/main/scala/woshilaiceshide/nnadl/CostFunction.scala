package woshilaiceshide.nnadl

import woshilaiceshide.nnadl.math._
import ConfigurableNetwork._

/**
 * use the quadratic cost when neurons in the output layer are linear, so the learning will not slow down.
 * if sigmoid neurons are used in the output layer, then it will slow down.
 */
class QuadraticCostFunction(val final_activation_function: ActivationFunction = LinearActivationFunction) extends CostFunction {

  def calc(a: Matrix, y: Matrix): Matrix = {
    val mapped = a.map_column(new Matrix.LineIteratorD() {
      def apply(line_number: Int, line: Line): Double = {
        var sum = 0.0d
        var i = 0
        while (i < line.length) {
          val aa = line(i)
          val yy = y(i)(0)
          sum = sum + Math.pow(yy - aa, 2) / 2
          i = i + 1
        }
        sum / 2
      }
    })
    Matrix.horizontal(mapped)
  }

  def delta(z: Matrix, a: Matrix, y: Matrix): Matrix = {
    val prime_to_a = a.map(new Matrix.CrossTransformer() {
      def apply(i: Int, j: Int, v: Double): Double = {
        val aa = v
        val yy = y(i)(0)
        aa - yy
      }
    })
    prime_to_a * final_activation_function.prime(z)
  }

}

class CrossEntropyCostFunction(val final_activation_function: ActivationFunction) extends CostFunction {

  def calc(a: Matrix, y: Matrix): Matrix = {
    val mapped = a.map_column(new Matrix.LineIteratorD() {
      def apply(line_number: Int, line: Line): Double = {
        var sum = 0.0d
        var i = 0
        while (i < line.length) {
          val aa = line(i)
          val yy = y(i)(0)
          sum = sum + Calc.nan_to_num(-1 * (yy * Math.log(aa) + (1 - yy) * Math.log(1 - aa)))
          i = i + 1
        }
        sum
      }
    })
    Matrix.horizontal(mapped)
  }

  def delta(z: Matrix, a: Matrix, y: Matrix): Matrix = {
    a.map(new Matrix.CrossTransformer() {
      def apply(i: Int, j: Int, v: Double): Double = {
        val aa = v
        val yy = y(i)(0)
        aa - yy
      }
    })
  }

}

object SoftmaxCostFunction extends CostFunction {

  //it should be softmax always
  val final_activation_function: ActivationFunction = SoftmaxActivationFunction

  def calc(a: Matrix, y: Matrix): Matrix = {
    val mapped = a.map_column(new Matrix.LineIteratorD() {
      def apply(line_number: Int, line: Line): Double = {
        var sum = 0.0d
        var i = 0
        while (i < line.length) {
          val aa = line(i)
          val yy = y(i)(0)
          sum = sum + Calc.nan_to_num(-1.0d * yy * Math.log(aa))
          i = i + 1
        }
        sum / 2
      }
    })
    Matrix.horizontal(mapped)
  }

  def delta(z: Matrix, a: Matrix, y: Matrix): Matrix = {
    a.map(new Matrix.CrossTransformer() {
      def apply(i: Int, j: Int, v: Double): Double = {
        val aa = v
        val yy = y(i)(0)
        aa - yy
      }
    })
  }

}

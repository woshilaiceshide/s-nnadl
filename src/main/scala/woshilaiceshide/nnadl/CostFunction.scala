package woshilaiceshide.nnadl

import woshilaiceshide.nnadl.math._

trait CostFunction {

  def calc(a: Matrix, y: Matrix): Matrix
  //∂C/∂z
  def delta(z: Matrix, a: Matrix, y: Matrix): Matrix

}

object QuadraticCostFunction extends CostFunction {

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
    prime_to_a * Calc.sigmoid_prime(z)
  }

}

object CrossEntropyCostFunction extends CostFunction {

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
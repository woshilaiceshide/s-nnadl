package woshilaiceshide.nnadl.math

object Calc {

  def sigmoid(z: Double): Double = 1.0d / (1.0d + Math.exp(-z))
  def sigmoid(z: Array[Double]): Array[Double] = z.map { sigmoid }
  def sigmoid(z: Matrix): Matrix = z.map { x => sigmoid(x) }

}
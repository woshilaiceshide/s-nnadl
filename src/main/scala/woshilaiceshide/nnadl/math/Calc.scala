package woshilaiceshide.nnadl.math

object Calc {

  /**
   * The sigmoid function
   */
  def sigmoid(z: Double): Double = 1.0d / (1.0d + Math.exp(-z))
  def sigmoid(z: Array[Double]): Array[Double] = z.map { sigmoid }
  def sigmoid(z: Matrix): Matrix = z.map { x => sigmoid(x) }

  /**
   * Derivative of the sigmoid function
   */
  def sigmoid_prime(z: Double): Double = sigmoid(z) * (1 - sigmoid(z))
  def sigmoid_prime(z: Array[Double]): Array[Double] = z.map { sigmoid_prime }
  def sigmoid_prime(z: Matrix): Matrix = z.map { x => sigmoid_prime(x) }

}
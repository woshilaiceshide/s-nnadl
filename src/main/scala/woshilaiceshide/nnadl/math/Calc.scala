package woshilaiceshide.nnadl.math

object Calc {

  import Matrix._

  /**
   * The sigmoid function
   */
  def sigmoid(z: Double): Double = 1.0d / (1.0d + Math.exp(-z))
  def sigmoid(z: Array[Double]): Array[Double] = {
    val a = new Array[Double](z.length)
    var i = 0
    while (i < a.length) {
      a(i) = sigmoid(z(i))
      i = i + 1
    }
    a
  }
  def sigmoid(z: Matrix): Matrix = {
    z.map {
      new CrossTransformer() { def apply(i: Int, j: Int, v: Double) = sigmoid(v) }
    }
  }

  /**
   * Derivative of the sigmoid function
   */
  def sigmoid_prime(z: Double): Double = sigmoid(z) * (1 - sigmoid(z))
  def sigmoid_prime(z: Array[Double]): Array[Double] = {
    val a = new Array[Double](z.length)
    var i = 0
    while (i < a.length) {
      a(i) = sigmoid_prime(z(i))
      i = i + 1
    }
    a
  }
  def sigmoid_prime(z: Matrix): Matrix = {
    z.map {
      new CrossTransformer() { def apply(i: Int, j: Int, v: Double) = sigmoid_prime(v) }
    }
  }

}
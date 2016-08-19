package woshilaiceshide.nnadl.math

object Calc {

  import Matrix._

  def sign(d: Double): Double = {
    Math.signum(d)
  }

  def sign(d: Array[Double]): Array[Double] = {
    val a = new Array[Double](d.length)
    var i = 0
    while (i < a.length) {
      a(i) = sign(d(i))
      i = i + 1
    }
    a
  }

  def sign(d: Matrix): Matrix = {
    d.map(new Matrix.ValueTransformer() {
      def apply(v: Double): Double = sign(v)
    })
  }

  def sign(d: Matrix, factor: Double): Matrix = {
    d.map(new Matrix.ValueTransformer() {
      def apply(v: Double): Double = sign(v) * factor
    })
  }

  def nan_to_num(d: Double): Double = d match {
    case Double.NegativeInfinity => Double.MinValue
    case Double.PositiveInfinity => Double.MaxValue
    case x if x.isNaN => 0.0d
    case x => x
  }
  def nan_to_num(d: Array[Double]): Array[Double] = {
    val a = new Array[Double](d.length)
    var i = 0
    while (i < a.length) {
      a(i) = nan_to_num(d(i))
      i = i + 1
    }
    a
  }
  def nan_to_num(z: Matrix): Matrix = {
    z.map {
      new CrossTransformer() { def apply(i: Int, j: Int, v: Double) = nan_to_num(v) }
    }
  }

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

  def tanh(z: Double): Double = Math.tanh(z)
  def tanh(z: Array[Double]): Array[Double] = {
    val a = new Array[Double](z.length)
    var i = 0
    while (i < a.length) {
      a(i) = tanh(z(i))
      i = i + 1
    }
    a
  }
  def tanh(z: Matrix): Matrix = {
    z.map {
      new CrossTransformer() { def apply(i: Int, j: Int, v: Double) = tanh(v) }
    }
  }

  def tanh_prime(z: Double): Double = { 1 / Math.pow(Math.cosh(z), 2) }
  def tanh_prime(z: Array[Double]): Array[Double] = {
    val a = new Array[Double](z.length)
    var i = 0
    while (i < a.length) {
      a(i) = tanh_prime(z(i))
      i = i + 1
    }
    a
  }
  def tanh_prime(z: Matrix): Matrix = {
    z.map {
      new CrossTransformer() { def apply(i: Int, j: Int, v: Double) = tanh_prime(v) }
    }
  }

  def softmax(z: Array[Double]): Array[Double] = {
    val a = new Array[Double](z.length)
    var sum = 0.0d

    {
      var i = 0
      while (i < a.length) {
        a(i) = Math.exp(z(i))
        sum = sum + a(i)
        i = i + 1
      }
    }

    {
      var i = 0
      while (i < a.length) {
        a(i) = a(i) / sum
        i = i + 1
      }
    }
    a
  }

  def softmax(z: Array[Double], c: Double): Array[Double] = {
    val a = new Array[Double](z.length)
    var sum = 0.0d

    {
      var i = 0
      while (i < a.length) {
        a(i) = Math.exp(z(i) * c)
        sum = sum + a(i)
        i = i + 1
      }
    }

    {
      var i = 0
      while (i < a.length) {
        a(i) = a(i) / sum
        i = i + 1
      }
    }
    a
  }

  def softmax(z: Matrix, c: Double): Matrix = {

    val a = z.zeros_with_the_same_shape()

    {
      var j = 0
      while (j < z.c_count) {

        var sum = 0.0d

        {
          var i = 0
          while (i < z.r_count) {
            val tmp = Math.exp(z(i)(j) * c)
            a.update(i, j, tmp)
            sum = sum + tmp
            i = i + 1
          }
        }

        {
          var i = 0
          while (i < z.r_count) {
            a.update(i, j, a(i)(j) / sum)
            i = i + 1
          }
        }

        j = j + 1
      }
    }

    a
  }

}
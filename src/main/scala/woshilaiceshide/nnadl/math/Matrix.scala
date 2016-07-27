package woshilaiceshide.nnadl.math

import woshilaiceshide.nnadl.util.Utility._

object Matrix {

  trait ValueTransformer { def apply(v: Double): Double }
  trait CrossTransformer { def apply(i: Int, j: Int, v: Double): Double }

  def apply(r_count: Int, c_count: Int) = new Matrix(r_count, c_count)

  def apply(r_count: Int, c_count: Int, transformer: CrossTransformer) = {
    new Matrix(r_count, c_count).map_directly { transformer }
  }

  def random(rnd: scala.util.Random, r: Int, c: Int): Matrix = apply(
    r,
    c,
    new CrossTransformer() {
      def apply(i: Int, j: Int, v: Double): Double = rnd.nextGaussian()
    })

  def vertical(a: Array[Double]): Matrix = {
    val array = new Array[Double](a.length)
    System.arraycopy(a, 0, array, 0, a.length)
    new Matrix(a.length, 1, array)
  }
  def vertical_unsafely(a: Array[Double]): Matrix = new Matrix(a.length, 1, a)

  def horizontal(a: Array[Double]): Matrix = {
    val array = new Array[Double](a.length)
    System.arraycopy(a, 0, array, 0, a.length)
    new Matrix(1, a.length, array)
  }
  def horizontal_unsafely(a: Array[Double]): Matrix = new Matrix(1, a.length, a)

  def wrap(a: Array[Array[Double]]): Matrix = {
    val r_count = a.length
    val c_count = if (0 == r_count) 0 else a(0).length
    apply(
      r_count,
      c_count,
      new CrossTransformer() {
        def apply(i: Int, j: Int, v: Double) = {
          a(i)(j)
        }
      })
  }

  def wrap(a: Array[Array[Int]]): Matrix = {
    val r_count = a.length
    val c_count = if (0 == r_count) 0 else a(0).length
    apply(
      r_count,
      c_count,
      new CrossTransformer() {
        def apply(i: Int, j: Int, v: Double) = {
          a(i)(j)
        }
      })
  }

  def argmax(a: Array[Double]): Int = {
    val ord = new Ordering[(Double, Int)] {
      def compare(x: (Double, Int), y: (Double, Int)): Int = x._1.compare(y._1)
    }
    (a zip a.length.range).max(ord)._2
  }

  def argmax(a: Array[Int]): Int = {
    val ord = new Ordering[(Int, Int)] {
      def compare(x: (Int, Int), y: (Int, Int)): Int = x._1.compare(y._1)
    }
    (a zip a.length.range).max(ord)._2
  }

  /**
   * @param axis: 0 stands for 'row', 1 stands for 'column'
   */
  def argmax(m: Matrix, axis: Int = 0): Array[Int] = {
    if (0 == axis) {
      m.map_column((c_number, line) => {
        argmax(line.toArray())
      })
    } else {
      m.map_row((r_number, line) => { argmax(line.toArray()) })
    }
  }

}

object Line {
  trait Retriever { def apply(i: Int): Double }
}
class Line(retriever: Line.Retriever, val length: Int, val is_row: Boolean) {

  final def is_column: Boolean = !is_row

  def toArray(a: Array[Double] = new Array(length)) = {
    length.range.map { x => a(x) = retriever(x) }
    a
  }

  def map[T](f: Double => T): IndexedSeq[T] = {
    length.range.map { x => f(retriever(x)) }
  }

  override def toString() = format("")

  def format(margin: String) = {

    import java.text._
    val formatter = new DecimalFormat("#0.000")

    def format(d: Double) = {
      if (d >= 0) s""" ${formatter.format(d)}"""
      else s"""${formatter.format(d)}"""
    }

    if (is_row) {
      s"""${margin}[${map { format }.mkString(", ")}]"""
    } else {
      length.range.map { x =>
        s"""|${format(retriever(x))}|"""
      }.map { margin + _ }.mkString(System.lineSeparator())
    }
  }

}

class Matrix protected[math] (val r_count: Int, val c_count: Int, private val array: Array[Double]) {

  import Matrix._

  override def toString() = format("")

  def format(margin: String): String = {

    import java.text._
    val formatter = new DecimalFormat("#0.000")

    def format(d: Double) = {
      if (d >= 0) s""" ${formatter.format(d)}"""
      else s"""${formatter.format(d)}"""
    }

    r_count.range.map { x =>
      s"""|${c_count.range.map { y => format(apply(x)(y)) }.mkString(", ")}|"""
    }.map { margin + _ }.mkString(System.lineSeparator())
  }

  def dim = (r_count, c_count)

  def column(j: Int): Line = {
    val retriever = new Line.Retriever {
      def apply(i: Int) = Matrix.this.apply(i)(j)
    }
    new Line(retriever, r_count, false)
  }
  def row(i: Int): Line = {
    val retriever = new Line.Retriever {
      def apply(j: Int) = Matrix.this.apply(i)(j)
    }
    new Line(retriever, r_count, false)
  }

  def toArray() = {
    val tmp = new Array[Array[Int]](r_count)
    tmp.length.range.map { i =>
      val row = new Array[Int](c_count)
      row.length.range.map { j =>
        row(j) = this(i)(j).toInt
      }
      tmp(i) = row
    }
    tmp
  }

  def map_row[T: scala.reflect.ClassTag](f: (Int, Line) => T) = {
    r_count.range.map { i => f(i, row(i)) }.toArray
  }

  def map_column[T: scala.reflect.ClassTag](f: (Int, Line) => T) = {
    c_count.range.map { j => f(j, column(j)) }.toArray
  }

  protected def map_directly(transformer: ValueTransformer): this.type = {
    var j = 0
    while (j < c_count) {
      var i = 0
      while (i < r_count) {
        this(i, j) = transformer(this(i)(j))
        i = i + 1
      }
      j = j + 1
    }
    this
  }

  protected def map_directly(transformer: CrossTransformer): this.type = {
    var j = 0
    while (j < c_count) {
      var i = 0
      while (i < r_count) {
        this(i, j) = transformer(i, j, this(i)(j))
        i = i + 1
      }
      j = j + 1
    }
    this
  }

  def of_the_same_dim(b: Matrix) = assert(r_count == b.r_count && c_count == b.c_count)

  def this(r_count: Int, c_count: Int) = this(r_count, c_count, new Array[Double](r_count * c_count))

  /**
   * the returned matrix shares the underlying array with me.
   */
  def reshape(r1: Int, c1: Int) = {
    assert(r1 * c1 == array.length)
    new Matrix(r1, c1, array)
  }

  def apply(i: Int)(j: Int) = { array((i) * c_count + j) }

  def update(i: Int, j: Int, v: Double) = { array((i) * c_count + j) = v }

  def zeros_with_the_same_shape() = new Matrix(r_count, c_count)

  /**
   * if this matrix if of 1 column of 1 row,
   * then the returened matrix is shared the inner array with this matrix.
   */
  def transpose() = {
    if (1 == r_count || 1 == c_count) {
      //val array1 = new Array[Double](array.length)
      //System.arraycopy(array, 0, array1, 0, array.length)
      //new Matrix(c_count, r_count, array1)
      new Matrix(c_count, r_count, array)
    } else {
      new Matrix(c_count, r_count).map_directly {
        new CrossTransformer() {
          def apply(i: Int, j: Int, v: Double) = { Matrix.this.apply(j)(i) }
        }
      }
    }
  }

  def map(f: ValueTransformer): Matrix = {
    val array1 = new Array[Double](array.length)
    var i = 0
    while (i < array1.length) {
      array1(i) = f(array(i))
      i = i + 1
    }
    new Matrix(r_count, c_count, array1)
  }

  def map(f: CrossTransformer): Matrix = {
    val tmp = new Matrix(r_count, c_count)
    var j = 0
    while (j < c_count) {
      var i = 0
      while (i < r_count) {
        tmp(i, j) = f(i, j, this(i)(j))
        i = i + 1
      }
      j = j + 1
    }
    tmp
  }

  def plus_directly(b: Matrix): Matrix = {
    of_the_same_dim(b)
    var i = 0
    while (i < array.length) {
      array(i) = array(i) + b.array(i)
      i = i + 1
    }
    this
  }

  def +(b: Matrix): Matrix = {
    of_the_same_dim(b)

    val array1 = new Array[Double](array.length)
    var i = 0
    while (i < array1.length) {
      array1(i) = array(i) + b.array(i)
      i = i + 1
    }
    new Matrix(r_count, c_count, array1)
  }
  def -(b: Matrix) = this + (b * -1)

  def +(d: Double) = {
    //map { _ + d }
    val array1 = new Array[Double](array.length)
    var i = 0
    while (i < array1.length) {
      array1(i) = array(i) + d
      i = i + 1
    }
    new Matrix(r_count, c_count, array1)
  }
  def -(d: Double) = this.+(-d)

  def dot(b: Matrix) = {
    val m = r_count
    val n = c_count
    val x = b.r_count
    val y = b.c_count
    assert(n == x)

    val tmp = new Matrix(m, y)

    var j = 0
    while (j < y) {
      var i = 0
      while (i < m) {
        tmp(i, j) = {
          var x = 0
          var sum = 0.0d
          while (x < c_count) {
            sum = sum + this(i)(x) * b(x)(j)
            x = x + 1
          }
          sum
        }
        i = i + 1
      }
      j = j + 1
    }

    tmp
  }
  def dot(d: Double) = {
    //map { _ * d }
    val array1 = new Array[Double](array.length)
    var i = 0
    while (i < array1.length) {
      array1(i) = array(i) * d
      i = i + 1
    }
    new Matrix(r_count, c_count, array1)

  }

  def multiple_directly(b: Matrix) = {
    of_the_same_dim(b)
    var i = 0
    while (i < array.length) {
      array(i) = array(i) * b.array(i)
      i = i + 1
    }
    this
  }
  def *(b: Matrix) = {
    of_the_same_dim(b)

    val array1 = new Array[Double](array.length)
    var i = 0
    while (i < array1.length) {
      array1(i) = array(i) * b.array(i)
      i = i + 1
    }
    new Matrix(r_count, c_count, array1)
  }
  def *(d: Double) = dot(d)

}

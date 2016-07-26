package woshilaiceshide.nnadl.math

import woshilaiceshide.nnadl.util.Utility._

object Matrix {

  def apply(r_count: Int, c_count: Int) = new Matrix(r_count, c_count)

  def apply(r_count: Int, c_count: Int, generator: (Int, Int) => Double) = {
    new Matrix(r_count, c_count).map_directly { (i, j, v) => generator(i, j) }
  }

  def random(rnd: scala.util.Random, r: Int, c: Int) = apply(r, c, (i, j) => rnd.nextGaussian())

  def vertical(a: Array[Double]) = apply(a.length, 1, (i, j) => a(i))
  def vertical_unsafely(a: Array[Double]) = new Matrix(a.length, 1, a)

  def horizontal(a: Array[Double]) = apply(1, a.length, (i, j) => a(j))
  def horizontal_unsafely(a: Array[Double]) = new Matrix(1, a.length, a)

  def wrap(a: Array[Array[Double]]) = {
    val r_count = a.length
    val c_count = if (0 == r_count) 0 else a(0).length
    apply(r_count, c_count, (i, j) => a(i)(j))
  }

  def wrap(a: Array[Array[Int]]) = {
    val r_count = a.length
    val c_count = if (0 == r_count) 0 else a(0).length
    apply(r_count, c_count, (i, j) => a(i)(j))
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
      m.map_column((c_number, line) => { argmax(line.toArray()) })
    }
  }

}

class Line(getter: Int => Double, val length: Int, val is_row: Boolean) {

  final def is_column: Boolean = !is_row

  def toArray(a: Array[Double] = new Array(length)) = {
    length.range.map { x => a(x) = getter(x) }
    a
  }

  def map[T](f: Double => T): IndexedSeq[T] = {
    length.range.map { x => f(getter(x)) }
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
        s"""|${format(getter(x))}|"""
      }.map { margin + _ }.mkString(System.lineSeparator())
    }
  }

}

class Matrix private (r_count: Int, c_count: Int, array: Array[Double]) {

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

  def this(r_count: Int, c_count: Int) = this(r_count, c_count, new Array[Double](r_count * c_count))

  /**
   * the returned matrix shares the underlying array with me.
   */
  def reshape(r1: Int, c1: Int) = {
    assert(r1 * c1 == array.length)
    new Matrix(r1, c1, array)
  }

  def dim = (r_count, c_count)

  def apply(i: Int)(j: Int) = { array((i) * c_count + j) }

  def update(i: Int, j: Int, v: Double) = { array((i) * c_count + j) = v }

  def column(j: Int): Line = new Line((i: Int) => apply(i)(j), r_count, false)
  def row(i: Int): Line = new Line((j: Int) => apply(i)(j), c_count, true)

  def zeros_with_the_same_shape() = new Matrix(r_count, c_count)

  def transpose() = Matrix(c_count, r_count, (i, j) => this(j)(i))

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

  def map(f: Double => Double): Matrix = {
    val tmp = new Matrix(r_count, c_count)
    for (i <- r_count.range; j <- c_count.range) {
      tmp(i, j) = f(this(i)(j))
    }
    tmp
  }

  def map(f: (Int, Int, Double) => Double): Matrix = {
    val tmp = new Matrix(r_count, c_count)
    for (i <- r_count.range; j <- c_count.range) {
      tmp(i, j) = f(i, j, this(i)(j))
    }
    tmp
  }

  def map_row[T: scala.reflect.ClassTag](f: (Int, Line) => T) = {
    r_count.range.map { i => f(i, row(i)) }.toArray
  }

  def map_column[T: scala.reflect.ClassTag](f: (Int, Line) => T) = {
    c_count.range.map { j => f(j, column(j)) }.toArray
  }

  private def map_directly(f: Double => Double): this.type = {
    for (i <- r_count.range; j <- c_count.range) {
      this(i, j) = f(this(i)(j))
    }
    this
  }

  private def map_directly(f: (Int, Int, Double) => Double): this.type = {
    for (i <- r_count.range; j <- c_count.range) {
      this(i, j) = f(i, j, this(i)(j))
    }
    this
  }

  def +(b: Matrix) = {
    assert(this.dim == b.dim)
    val tmp = new Matrix(r_count, c_count)
    for (i <- r_count.range; j <- c_count.range) {
      tmp(i, j) = this(i)(j) + b(i)(j)
    }
    tmp
  }
  def -(b: Matrix) = this + (b * -1)

  def +(d: Double) = map { _ + d }
  def -(d: Double) = map { _ - d }

  def dot(b: Matrix) = {
    val (m, n) = this.dim
    val (x, y) = b.dim

    assert(n == x)

    val tmp = new Matrix(m, y)
    for (i <- m.range; j <- y.range) {
      val r = this(i) _
      def c(index: Int) = b(index)(j)
      tmp(i, j) = c_count.range.map { x => r(x) * c(x) }.sum
    }
    tmp
  }
  def dot(d: Double) = map { _ * d }

  def *(b: Matrix) = {
    assert(this.dim == b.dim)
    this.map { (i, j, v) => v * b(i)(j) }
  }

  def *(d: Double) = map { _ * d }

}
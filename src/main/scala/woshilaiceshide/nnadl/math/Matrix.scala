package woshilaiceshide.nnadl.math

import woshilaiceshide.nnadl.util.Utility._

object Matrix {

  def apply(r_count: Int, c_count: Int) = new Matrix(r_count, c_count)

  def apply(r_count: Int, c_count: Int, generator: (Int, Int) => Double) = {
    new Matrix(r_count, c_count).map_directly { (i, j, v) => generator(i, j) }
  }

  def random(rnd: scala.util.Random, r: Int, c: Int) = {
    apply(r, c, (i, j) => rnd.nextGaussian())
  }

  def vertical(a: Array[Double]) = apply(a.length, 1, (i, j) => a(i))

  def horizontal(a: Array[Double]) = apply(1, a.length, (i, j) => a(j))

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

class Matrix(r_count: Int, c_count: Int) {

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

  private val array = new Array[Double](r_count * c_count)

  def dim = (r_count, c_count)

  def apply(i: Int)(j: Int) = { array((i) * c_count + j) }

  def update(i: Int, j: Int, v: Double) = { array((i) * c_count + j) = v }

  def column(j: Int): Line = new Line((i: Int) => apply(i)(j), r_count, false)
  def row(i: Int): Line = new Line((j: Int) => apply(i)(j), c_count, true)

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

  def map_row[T](i: Int, f: (Int, Double) => T) = {

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

  def +(d: Double) = map { _ + d }

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

  def *(b: Matrix) = dot(b)

  def *(d: Double) = map { _ * d }

}
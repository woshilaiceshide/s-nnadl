package woshilaiceshide.nnadl.util

trait Utility {

}

object Utility extends Utility {

  implicit class Range(val x: Int) extends AnyVal { def range = 0 until x }

  def using[T <: { def close(): Unit }, U](generator: => T)(op: T => U): U = {
    val empty = null.asInstanceOf[T]
    var resource: T = empty
    try {
      resource = generator
      op(resource)
    } finally {
      if (null != resource) { resource.close(); resource = empty }
    }
  }

  def cut_array[T: scala.reflect.ClassTag](a: Array[T], cut_point: Int) = {
    if (a.length <= cut_point) {
      (a, new Array[T](0))

    } else if (0 == cut_point) {
      (new Array[T](0), a)

    } else {
      val a0 = new Array[T](cut_point)
      val a1 = new Array[T](a.length - cut_point)
      System.arraycopy(a, 0, a0, 0, a0.length)
      System.arraycopy(a, cut_point, a1, 0, a1.length)
      (a0, a1)
    }
  }

  /**
   * println(Utility.group_array(Array('a', 'b', 'c', 'd', "e"), 30).toList.map { x => "[" + x.mkString(", ") + "]" })
   * println(Utility.group_array(Array('a', 'b', 'c', 'd', "e"), 2).toList.map { x => "[" + x.mkString(", ") + "]" })
   * println(Utility.group_array(Array('a', 'b', 'c', 'd', "e"), 5).toList.map { x => "[" + x.mkString(", ") + "]" })
   */
  def group_array[T: scala.reflect.ClassTag](a: Array[T], group_count: Int) = {
    val s = a.length / group_count
    val m = a.length % group_count

    if (a.length < group_count) {
      a.map { x => Array(x) }
    } else if (m == 0) {
      a.grouped(s).toArray
    } else {
      val tmp1 = m.range.map { i =>
        a.slice(i * (s + 1), (i + 1) * (s + 1)).toArray
      }
      val tmp2 = a.drop(m * (s + 1)).grouped(s)
      (tmp1 ++ tmp2).toArray
    }

  }

}
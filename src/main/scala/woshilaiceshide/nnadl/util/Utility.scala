package woshilaiceshide.nnadl.util

trait Utility extends ArrayUtility {

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

}
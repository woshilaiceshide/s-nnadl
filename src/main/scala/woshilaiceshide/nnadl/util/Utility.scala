package woshilaiceshide.nnadl.util

trait Utility {

}

object Utility extends Utility {

  implicit class Range(val x: Int) extends AnyVal { def range = 0 until x }

}
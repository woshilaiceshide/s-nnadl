package woshilaiceshide.nnadl.util

trait ArrayUtility {}

object ArrayUtility extends ArrayUtility {

  implicit class ArrayWrapper[T](val a: Array[T]) extends AnyVal {

    /**
     * the ith element from the head
     */
    def h(i: Int) = a(i)
    def set_h(i: Int, t: T) = { a(i) = t }
    /**
     * the (-i)th element from the tail
     */
    def t(i: Int) = a(a.length - 1 + i)
    def set_t(i: Int, t: T) = { a(a.length - 1 + i) = t }

    def cut_at_point(cut_point: Int)(implicit ct: scala.reflect.ClassTag[T]) = {
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
     *
     * println(Array('a', 'b', 'c', 'd', "e").cut_to_groups(1).toList.map { x => "[" + x.mkString(", ") + "]" })
     * println(Array('a', 'b', 'c', 'd', "e").cut_to_groups(2).toList.map { x => "[" + x.mkString(", ") + "]" })
     * println(Array('a', 'b', 'c', 'd', "e").cut_to_groups(3).toList.map { x => "[" + x.mkString(", ") + "]" })
     * println(Array('a', 'b', 'c', 'd', "e").cut_to_groups(4).toList.map { x => "[" + x.mkString(", ") + "]" })
     * println(Array('a', 'b', 'c', 'd', "e").cut_to_groups(5).toList.map { x => "[" + x.mkString(", ") + "]" })
     * println(Array('a', 'b', 'c', 'd', "e").cut_to_groups(6).toList.map { x => "[" + x.mkString(", ") + "]" })
     */
    def cut_to_groups(group_count: Int)(implicit ct: scala.reflect.ClassTag[T]): Array[Array[T]] = {
      val s = a.length / group_count
      val m = a.length % group_count

      if (a.length < group_count) {
        a.map { x => Array(x) }

      } else if (m == 0) {
        a.grouped(s).toArray

      } else {

        val tmp = new Array[Array[T]](group_count)

        var i = 0
        while (i < m) {
          tmp(i) = new Array[T](s + 1)
          System.arraycopy(a, i * (s + 1), tmp(i), 0, s + 1)
          i = i + 1
        }

        val dropped = m * (s + 1)

        while (i < group_count) {
          tmp(i) = new Array[T](s)
          System.arraycopy(a, dropped + (i - m) * s, tmp(i), 0, s)
          i = i + 1
        }

        tmp
      }

    }

  }

}


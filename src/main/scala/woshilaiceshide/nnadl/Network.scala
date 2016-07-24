package woshilaiceshide.nnadl

import woshilaiceshide.nnadl.util._
import woshilaiceshide.nnadl.util.Utility._
import woshilaiceshide.nnadl.math._

object Network {}

class Network(sizes: Seq[Int]) {

  import Network._

  override def toString() = {
    val ident = "   "
    val formatted_biases = {
      biases.length.range.map { i =>
        s"""${ident}layer ${i + 1}:
${biases(i).format(ident + ident)}"""
      }
    }

    val formatted_weights = {
      weights.length.range.map { i =>
        s"""${ident}layer ${i + 1}:
${weights(i).format(ident + ident)}"""
      }
    }

    s"""layers count: ${num_layers}
layers sizes: ${sizes.mkString(", ")}
current biases: 
${formatted_biases.mkString(System.lineSeparator())}
current weights:
${formatted_weights.mkString(System.lineSeparator())}"""
  }

  private val rnd = new scala.util.Random()

  val num_layers = sizes.length

  val biases = sizes.slice(1, sizes.length).toArray.map { layer_size =>
    val tmp = Matrix.random(rnd, layer_size, 1)
    tmp
  }

  val weights = (sizes.slice(0, sizes.length - 1) zip sizes.slice(1, sizes.length)).toArray.map { one =>
    val (x, y) = one
    Matrix.random(rnd, y, x)
  }

  def feedforward(input: Array[Double]) = {
    val zipped = biases zip weights
    @scala.annotation.tailrec
    def iterate(i: Int, a: Matrix): Matrix = {
      if (i < zipped.length) {
        val (b, w) = zipped(i)
        iterate(i + 1, Calc.sigmoid(w.dot(a) + b))
      } else {
        a
      }
    }
    iterate(0, Matrix.vertical(input))
  }

  //TODO
  def SGD() = throw new scala.NotImplementedError

}



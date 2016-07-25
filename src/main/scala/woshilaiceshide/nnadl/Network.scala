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

  def SGD(
    training_data: Array[MnistLoader.MnistRecord1],
    epochs: Int,
    mini_batch_size: Int,
    eta: Double,
    test_data: Option[Array[MnistLoader.MnistRecord2]] = None) = {
    val n_test = test_data.map { _.length }.getOrElse(0)
    val n = training_data.length
    epochs.range.map { j =>
      val shuffled = rnd.shuffle(training_data.toSeq).toArray
      val mini_batches = 0.until(n, mini_batch_size).map { k =>
        training_data.slice(k, k + mini_batch_size)
      }

      mini_batches.map { mini_batch => update_mini_batch(mini_batch, eta) }

      test_data match {
        case Some(test_data) =>
          println(s"""Epoch ${j}: ${evaluate(test_data)} / ${n_test}""")
        case None =>
          println(s"""Epoch ${j} complete""")
      }

    }

  }

  //TODO
  def update_mini_batch(mini_batch: Array[MnistLoader.MnistRecord1], eta: Double) = {

  }

  //TODO 
  def evaluate(test_data: Array[MnistLoader.MnistRecord2]) = {

  }

}



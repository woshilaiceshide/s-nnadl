package woshilaiceshide.nnadl

import woshilaiceshide.nnadl.util._
import woshilaiceshide.nnadl.util.Utility._
import woshilaiceshide.nnadl.math._

object Network {}

class Network(sizes: Seq[Int]) {

  assert(sizes.length >= 2)

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

  protected val rnd = new scala.util.Random()

  val num_layers = sizes.length

  var biases = sizes.slice(1, sizes.length).map { layer_size =>
    Matrix.random(rnd, layer_size, 1)
  }.toArray

  var weights = (sizes.slice(0, sizes.length - 1) zip sizes.slice(1, sizes.length)).map { one =>
    val (x, y) = one
    Matrix.random(rnd, y, x)
  }.toArray

  def feedforward(input: Array[Double]): Matrix = feedforward(Matrix.vertical(input))

  def feedforward(input: Matrix): Matrix = {
    var a = input
    val zipped = biases zip weights
    zipped.length.range.map { i =>
      val (b, w) = zipped(i)
      a = Calc.sigmoid(w.dot(a) + b)
    }
    a
  }

  def SGD(
    training_data: Array[MnistLoader.MnistRecord1],
    epochs: Int,
    mini_batch_size: Int,
    eta: Double,
    test_data: Option[Array[MnistLoader.MnistRecord2]]): Unit = {
    val n_test = test_data.map { _.length }.getOrElse(0)
    val n = training_data.length
    epochs.range.map { j =>
      val shuffled = rnd.shuffle(training_data.toSeq).toArray
      val mini_batches = 0.until(n, mini_batch_size).map { k =>
        shuffled.slice(k, k + mini_batch_size)
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

  def update_mini_batch(mini_batch: Array[MnistLoader.MnistRecord1], eta: Double) = {
    var nabla_b = biases.map { b => b.zeros_with_the_same_shape() }
    var nabla_w = weights.map { w => w.zeros_with_the_same_shape() }
    mini_batch.map { x =>
      val MnistLoader.MnistRecord1(image, label) = x
      val (delta_nabla_b, delta_nabla_w) = backprop(image, label)
      nabla_b = (nabla_b zip delta_nabla_b).map { x =>
        val (nb, dnb) = x
        nb + dnb
      }.toArray

      nabla_w = (nabla_w zip delta_nabla_w).map { x =>
        val (nw, dnw) = x
        nw + dnw
      }
    }

    weights = (weights zip nabla_w).map { x =>
      val (w, nw) = x
      w - nw * (eta / mini_batch.length)
    }.toArray

    biases = (biases zip nabla_b).map { x =>
      val (b, nb) = x
      b - nb * (eta / mini_batch.length)
    }.toArray

  }

  def backprop(x: Matrix, y: Matrix) = {

    val nabla_b = biases.map { b => b.zeros_with_the_same_shape() }
    val nabla_w = weights.map { w => w.zeros_with_the_same_shape() }

    val activations = new Array[Matrix](num_layers); activations(0) = x
    val zs = new Array[Matrix](num_layers - 1)
    val zipped = (biases zip weights)
    zipped.length.range.map { i =>
      val (b, w) = zipped(i)
      val z = w.dot(activations(i)) + b
      zs(i) = z
      activations(i + 1) = Calc.sigmoid(z)
    }
    var delta = cost_derivative(activations.last, y) * Calc.sigmoid_prime(zs.last)
    nabla_b(nabla_b.length - 1) = delta
    nabla_w(nabla_w.length - 1) = delta.dot(activations(activations.length - 2).transpose())

    (2 until num_layers).map { l =>
      val z = zs(zs.length - l)
      val sp = Calc.sigmoid_prime(z)
      delta = weights(weights.length - l + 1).transpose().dot(delta) * sp
      nabla_b(nabla_b.length - l) = delta
      nabla_w(nabla_w.length - l) = delta.dot(activations(activations.length - l - 1).transpose())
    }

    (nabla_b, nabla_w)
  }

  def cost_derivative(output_activations: Matrix, y: Matrix) = {
    output_activations - y
  }

  def evaluate(test_data: Array[MnistLoader.MnistRecord2]) = {
    val test_results = test_data.map { record =>
      (Matrix.argmax(feedforward(record.image), 0)(0), record.label)
    }
    test_results.count(x => x._1 == x._2)
  }

}



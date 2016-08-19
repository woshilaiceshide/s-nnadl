
package woshilaiceshide.nnadl

import woshilaiceshide.nnadl.util._
import woshilaiceshide.nnadl.util.Utility._
import woshilaiceshide.nnadl.util.ArrayUtility._
import woshilaiceshide.nnadl.math._

import woshilaiceshide.nnadl.mnist._

object MultiThreadingNetwork {}

import ConfigurableNetwork._

class MultiThreadingNetwork(sizes: Array[Int], configurator: Configurator) extends ConfigurableNetwork(sizes, configurator) {

  import configurator._

  private val STOP_SIGNAL = new Runnable() { def run() {} }

  private final class Worker extends Thread {

    private val tasks = new java.util.concurrent.LinkedBlockingQueue[Runnable]()
    def execute(r: Runnable) = tasks.put(r)
    def shutdown() = tasks.put(STOP_SIGNAL)

    override def run(): Unit = {
      @scala.annotation.tailrec
      def run1(): Unit = {
        val r = tasks.take()
        r.run()
        if (r ne STOP_SIGNAL) run1()
      }
      run1()
    }
  }

  def SGD(
    training_data: Array[NRecord],
    epochs: Int,
    mini_batch_size: Int,
    eta: Double,
    test_data: Option[Array[NRecord]], thread_count: Int): Unit = {

    if (thread_count <= 1) {
      SGD(training_data, epochs, mini_batch_size, eta, test_data)

    } else {

      val n_test = test_data.map { _.length }.getOrElse(0)
      val n = training_data.length

      val workers = Array.fill[Worker](thread_count)(new Worker())
      workers.map { _.start() }
      val grouped_test_data = test_data.map { x => x.cut_to_groups(workers.length) }

      epochs.range.map { j =>

        val start = System.currentTimeMillis()

        val shuffled = training_data.shuffle_directly(configurator.rnd)
        val mini_batches = shuffled.grouped_with_fixed_size(mini_batch_size)

        mini_batches.map { mini_batch => update_mini_batch(mini_batch, eta, training_data.length, workers) }

        grouped_test_data match {
          case Some(grouped_test_data) if 0 == j % 5 =>
            val medias = new Array[Int](grouped_test_data.length)
            val latch = new java.util.concurrent.CountDownLatch(grouped_test_data.length)
            grouped_test_data.length.range.map { i =>

              workers(i).execute(new Runnable() {
                def run() {
                  val count = evaluate(grouped_test_data(i))
                  medias(i) = count
                  latch.countDown()
                }
              })

            }
            latch.await()
            val end = System.currentTimeMillis()
            println(s"""Epoch ${j}: ${end - start}ms ${medias.reduce(_ + _)} / ${n_test}""")
          case _ =>
            val end = System.currentTimeMillis()
            println(s"""Epoch ${j}: ${end - start}ms completed""")
        }
      }
      workers.map { _.shutdown() }
      workers.map { _.join() }
    }
  }

  def update_mini_batch(mini_batch: Array[NRecord], eta: Double, n: Int, workers: Array[Worker]) = {

    val grouped = mini_batch.cut_to_groups(workers.length)
    val medias = new Array[(Array[Matrix], Array[Matrix])](grouped.length)
    val latch = new java.util.concurrent.CountDownLatch(grouped.length)
    grouped.length.range.map { i =>
      workers(i).execute(new Runnable() {
        def run() {
          val nabla_b = zeros_with_the_same_shape(biases)
          val nabla_w = zeros_with_the_same_shape(weights)

          {
            def work() = {
              var x1 = 0
              while (x1 < grouped(i).length) {
                val x = grouped(i)(x1)
                val NRecord(image, label) = x
                val (delta_nabla_b, delta_nabla_w) = backprop(image, label)

                {
                  var x = 0
                  while (x < nabla_b.length) {
                    val nb = nabla_b(x); val dnb = delta_nabla_b(x);
                    nb.plus_directly(dnb)
                    x = x + 1
                  }
                }

                {
                  var x = 0
                  while (x < nabla_w.length) {
                    val nw = nabla_w(x); val dnw = delta_nabla_w(x);
                    nw.plus_directly(dnw)
                    x = x + 1
                  }
                }

                x1 = x1 + 1
              }
            }
            work()
          }
          medias(i) = (nabla_b, nabla_w)
          latch.countDown()
        }
      })
    }
    latch.await()
    def add(a: Array[Matrix], b: Array[Matrix]) = {
      var i = 0
      while (i < a.length) {
        a(i).plus_directly(b(i))
        i = i + 1
      }
      a
    }
    val (nabla_b, nabla_w) = {
      val b = medias(0)._1
      val w = medias(0)._2
      var i = 1
      while (i < medias.length) {
        add(b, medias(i)._1)
        add(w, medias(i)._2)
        i = i + 1
      }
      (b, w)
    }

    def learn_from_backprops() {
      val learned_layers = sizes.length - 1
      var i = 0
      while (i < learned_layers) {
        //weights(i).substract_directly(nabla_w(i), (eta / mini_batch.length))
        weights(i).substract_directly(regularization.prime(weights(i), n), eta).substract_directly(nabla_w(i), (eta / mini_batch.length))
        biases(i).substract_directly(nabla_b(i), (eta / mini_batch.length))
        i = i + 1
      }
    }

    learn_from_backprops()
  }

}



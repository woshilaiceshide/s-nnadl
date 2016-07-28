
package woshilaiceshide.nnadl

import woshilaiceshide.nnadl.util._
import woshilaiceshide.nnadl.util.Utility._
import woshilaiceshide.nnadl.math._

import woshilaiceshide.nnadl.mnist._

object MultiThreadingNetwork {}

class MultiThreadingNetwork(sizes: Array[Int]) extends Network(sizes) {

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
    training_data: Array[MnistRecord1],
    epochs: Int,
    mini_batch_size: Int,
    eta: Double,
    test_data: Option[Array[MnistRecord2]], thread_count: Int): Unit = {

    if (thread_count <= 1) {
      SGD(training_data, epochs, mini_batch_size, eta, test_data)

    } else {

      val n_test = test_data.map { _.length }.getOrElse(0)
      val n = training_data.length

      val workers = Array.fill[Worker](thread_count)(new Worker())
      workers.map { _.start() }
      val grouped_test_data = test_data.map { x => Utility.group_array(x, workers.length) }

      epochs.range.map { j =>

        val start = System.currentTimeMillis()

        val shuffled = rnd.shuffle(training_data.toSeq).toArray
        val mini_batches = 0.until(n, mini_batch_size).map { k =>
          shuffled.slice(k, k + mini_batch_size)
        }

        mini_batches.map { mini_batch => update_mini_batch(mini_batch, eta, workers) }

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

  def update_mini_batch(mini_batch: Array[MnistRecord1], eta: Double, workers: Array[Worker]) = {

    val grouped = Utility.group_array(mini_batch, workers.length)
    val medias = new Array[(Array[Matrix], Array[Matrix])](grouped.length)
    val latch = new java.util.concurrent.CountDownLatch(grouped.length)
    grouped.length.range.map { i =>
      workers(i).execute(new Runnable() {
        def run() {
          val nabla_b = biases.map { b => b.zeros_with_the_same_shape() }
          val nabla_w = weights.map { w => w.zeros_with_the_same_shape() }

          {
            def work() = {
              var x1 = 0
              while (x1 < grouped(i).length) {
                val x = grouped(i)(x1)
                val MnistRecord1(image, label) = x
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

    weights = (weights zip nabla_w).map { x =>
      val (w, nw) = x
      w - (nw * (eta / mini_batch.length))
    }

    biases = (biases zip nabla_b).map { x =>
      val (b, nb) = x
      b - (nb * (eta / mini_batch.length))
    }

  }

}



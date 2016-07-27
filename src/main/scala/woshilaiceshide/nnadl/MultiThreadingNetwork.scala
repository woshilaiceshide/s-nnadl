
package woshilaiceshide.nnadl

import woshilaiceshide.nnadl.util._
import woshilaiceshide.nnadl.util.Utility._
import woshilaiceshide.nnadl.math._

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
    training_data: Array[MnistLoader.MnistRecord1],
    epochs: Int,
    mini_batch_size: Int,
    eta: Double,
    test_data: Option[Array[MnistLoader.MnistRecord2]], thread_count: Int): Unit = {

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

  def update_mini_batch(mini_batch: Array[MnistLoader.MnistRecord1], eta: Double, workers: Array[Worker]) = {

    val grouped = Utility.group_array(mini_batch, workers.length)
    val medias = new Array[(Array[Matrix], Array[Matrix])](grouped.length)
    val latch = new java.util.concurrent.CountDownLatch(grouped.length)
    grouped.length.range.map { i =>
      workers(i).execute(new Runnable() {
        def run() {
          var nabla_b = biases.map { b => b.zeros_with_the_same_shape() }
          var nabla_w = weights.map { w => w.zeros_with_the_same_shape() }

          {
            def work1() = {
              grouped(i).map { x =>
                val MnistLoader.MnistRecord1(image, label) = x
                val (delta_nabla_b, delta_nabla_w) = backprop(image, label)
                nabla_b = (nabla_b zip delta_nabla_b).map { x =>
                  val (nb, dnb) = x
                  nb + dnb
                }

                nabla_w = (nabla_w zip delta_nabla_w).map { x =>
                  val (nw, dnw) = x
                  nw + dnw
                }
              }
            }

            def work2() = {
              var x1 = 0
              while (x1 < grouped(i).length) {
                val x = grouped(i)(x1)
                val MnistLoader.MnistRecord1(image, label) = x
                val (delta_nabla_b, delta_nabla_w) = backprop(image, label)
                (nabla_b zip delta_nabla_b).map { x =>
                  val (nb, dnb) = x
                  nb.plus_directly(dnb)
                }

                (nabla_w zip delta_nabla_w).map { x =>
                  val (nw, dnw) = x
                  nw.plus_directly(dnw)
                }
                x1 = x1 + 1
              }
            }
            work2()
          }
          medias(i) = (nabla_b, nabla_w)
          latch.countDown()
        }
      })
    }
    latch.await()
    def add(a: Array[Matrix], b: Array[Matrix]) = (a zip b).map { x => x._1 + x._2 }
    val (nabla_b, nabla_w) = medias.reduce((x, y) => (add(x._1, y._1), add(x._2, y._2)))

    weights = (weights zip nabla_w).map { x =>
      val (w, nw) = x
      w - nw * (eta / mini_batch.length)
    }.toArray

    biases = (biases zip nabla_b).map { x =>
      val (b, nb) = x
      b - nb * (eta / mini_batch.length)
    }.toArray

  }

}



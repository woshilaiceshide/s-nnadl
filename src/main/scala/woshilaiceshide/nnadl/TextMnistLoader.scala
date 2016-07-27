package woshilaiceshide.nnadl

import java.io._
import java.util.zip._

import woshilaiceshide.nnadl.math._
import woshilaiceshide.nnadl.util.Utility._

object TextMnistLoader {

  import MnistLoader._

  val default_folder = (new File(System.getProperty("user.dir"), "data")).getAbsolutePath

  def load_data_wrapper(folder: String = default_folder) = {

    def iterate_line_by_line[T: scala.reflect.ClassTag](file: String)(op: String => T): Array[T] = {
      def find_file(f: String) = (new java.io.File(folder, f)).getAbsolutePath
      using(new FileInputStream(find_file(file))) { input =>
        using(new GZIPInputStream(input, 4096 * 1024)) { gzip =>
          using(new BufferedReader(new InputStreamReader(gzip))) { br =>
            val buffer = new scala.collection.mutable.ListBuffer[T]()
            var line = br.readLine()
            while (line != null) {
              buffer += op(line)
              line = br.readLine()
            }
            buffer.toArray
          }
        }
      }
    }

    val training_data = iterate_line_by_line("training.txt.gz") { line =>
      val Array(s0, s1) = line.split('|')
      val label = Matrix.argmax(s0.split(',').map { _.toDouble }.toArray)
      val image = Matrix.wrap(Array(s1.split(',').map { _.toDouble }.toArray)).transpose()
      MnistRecord1(image, vectorized_result(label))
    }

    val validation_data = iterate_line_by_line("validation.txt.gz") { line =>
      val Array(s0, s1) = line.split('|')
      val label = s0.toDouble.toInt
      val image = Matrix.wrap(Array(s1.split(',').map { _.toDouble }.toArray)).transpose()
      MnistRecord2(image, label)
    }

    val test_data = iterate_line_by_line("test.txt.gz") { line =>
      val Array(s0, s1) = line.split('|')
      val label = s0.toDouble.toInt
      val image = Matrix.wrap(Array(s1.split(',').map { _.toDouble }.toArray)).transpose()
      MnistRecord2(image, label)
    }

    MnistDataSet(training_data, validation_data, test_data)
  }

}
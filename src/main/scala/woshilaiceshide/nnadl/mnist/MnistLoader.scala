package woshilaiceshide.nnadl.mnist

import java.io._
import java.util.zip._

import woshilaiceshide.nnadl._
import woshilaiceshide.nnadl.math._
import woshilaiceshide.nnadl.util.Utility._

/**
 * read data listed on http://yann.lecun.com/exdb/mnist/
 */
object MnistLoader {

  val default_folder = (new File(System.getProperty("user.dir"), "data")).getAbsolutePath

  val file_t10_images_gz = """t10k-images-idx3-ubyte.gz"""
  val file_t10_images = """t10k-images.idx3-ubyte"""
  val file_t10_labels = """t10k-labels-idx1-ubyte.gz"""
  val file_train_images_gz = """train-images-idx3-ubyte.gz"""
  val file_train_images = """train-images.idx3-ubyte"""
  val file_train_labels = """train-labels-idx1-ubyte.gz"""

  private def read_int_with_msb(stream: InputStream) = {
    val ch1 = stream.read()
    val ch2 = stream.read()
    val ch3 = stream.read()
    val ch4 = stream.read()

    if ((ch1 | ch2 | ch3 | ch4) < 0) throw new EOFException();
    (ch1 << 24) + (ch2 << 16) + (ch3 << 8) + (ch4 << 0)
  }

  private def read_unsigned_byte(stream: InputStream) = stream.read()

  /**
   *
   * TRAINING SET IMAGE FILE (train-images-idx3-ubyte):
   * [offset] [type]          [value]          [description]
   * 0000     32 bit integer  0x00000803(2051) magic number
   * 0004     32 bit integer  60000            number of images
   * 0008     32 bit integer  28               number of rows
   * 0012     32 bit integer  28               number of columns
   * 0016     unsigned byte   ??               pixel
   * 0017     unsigned byte   ??               pixel
   * ........
   * xxxx     unsigned byte   ??               pixel
   *
   * Pixels are organized row-wise. Pixel values are 0 to 255. 0 means background (white), 255 means foreground (black).
   */
  def read_images(path: String, gzipped: Boolean = false) = {

    def read(input_stream: InputStream) = {
      def next_int() = read_int_with_msb(input_stream)
      def next_byte() = read_unsigned_byte(input_stream)

      val ROW_COUNT = 28
      val COLUMN_COUNT = 28

      assert(2051 == next_int())
      val number_of_images = next_int()
      assert(ROW_COUNT == next_int())
      assert(COLUMN_COUNT == next_int())

      val images = new Array[Array[Array[Int]]](number_of_images)
      images.length.range.map { x =>
        val image = new Array[Array[Int]](ROW_COUNT)
        image.length.range.map { i =>
          val row = new Array[Int](COLUMN_COUNT)
          row.length.range.map { j => row(j) = next_byte() }
          image(i) = row
        }
        images(x) = image
      }
      images
    }

    if (gzipped) {
      using(new FileInputStream(path)) { input =>
        using(new GZIPInputStream(input, 4096 * 1024)) { gzip =>
          read(gzip)
        }
      }
    } else {
      using(new FileInputStream(path)) { input =>
        using(new BufferedInputStream(input, 4096 * 1024)) { gzip =>
          read(gzip)
        }
      }
    }
  }

  /**
   *
   * TRAINING SET LABEL FILE (train-labels-idx1-ubyte):
   * [offset] [type]          [value]          [description]
   * 0000     32 bit integer  0x00000801(2049) magic number (MSB first)
   * 0004     32 bit integer  60000            number of items
   * 0008     unsigned byte   ??               label
   * 0009     unsigned byte   ??               label
   * ........
   * xxxx     unsigned byte   ??               label
   *
   * The labels values are 0 to 9.
   */
  def read_labels(path: String) = {
    using(new FileInputStream(path)) { input =>
      using(new GZIPInputStream(input)) { gzip =>

        def next_int() = read_int_with_msb(gzip)
        def next_byte() = read_unsigned_byte(gzip)

        assert(2049 == next_int())
        val number_of_items = next_int()

        val labels = new Array[Int](number_of_items)
        labels.length.range.map { x =>
          labels(x) = next_byte()
        }
        labels
      }
    }
  }

  def vectorized_result(j: Int): Matrix = {
    val m = Matrix(10, 1)
    m(j, 0) = 1.0d
    m
  }

  def load_data(factor: Double, folder: String = default_folder): MnistRawDataSet = {

    def find_file(f: String) = (new java.io.File(folder, f)).getAbsolutePath

    def generate_raw_data(image_file: String, label_file: String) = {
      val images = {
        val tmp = read_images(find_file(image_file))
        val result = new Array[Matrix](tmp.length)
        result.length.range.map { i =>
          result(i) = Matrix
            .wrap(tmp(i))
            .map_directly(new Matrix.ValueTransformer() {
              def apply(v: Double): Double = 1.0d * v / 255 / factor
            })
        }
        result
      }
      val labels = read_labels(find_file(label_file))
      MnistRawData(images, labels)
    }

    val (training_data, validation_data) = generate_raw_data(file_train_images, file_train_labels).split(50000)
    val test_data = generate_raw_data(file_t10_images, file_t10_labels)

    MnistRawDataSet(training_data, validation_data, test_data)

  }

  def load_data_wrapper(factor: Double, folder: String = default_folder) = {

    val MnistRawDataSet(tr_d, va_d, te_d) = load_data(factor: Double, folder)

    val training_inputs = tr_d.images.map { _.transpose().reshape(784, 1) }
    val training_results = tr_d.labels.map { vectorized_result(_) }
    val training_data = (training_inputs zip training_results).map { x => NRecord(x._1, x._2) }

    val validation_inputs = va_d.images.map { _.transpose().reshape(784, 1) }
    val validation_results = va_d.labels.map { vectorized_result(_) }
    val validation_data = (validation_inputs zip validation_results).map { x => NRecord(x._1, x._2) }

    val test_inputs = te_d.images.map { _.transpose().reshape(784, 1) }
    val test_results = te_d.labels.map { vectorized_result(_) }
    val test_data = (test_inputs zip test_results).map { x => NRecord(x._1, x._2) }

    MnistDataSet(training_data, validation_data, test_data)
  }

}




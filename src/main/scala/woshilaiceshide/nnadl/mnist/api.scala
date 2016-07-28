package woshilaiceshide.nnadl.mnist

import woshilaiceshide.nnadl.math._
import woshilaiceshide.nnadl.util.Utility._

import java.io._

final case class MnistRawData(images: Array[Matrix], labels: Array[Int]) {
  private[nnadl] def split(howmany: Int) = {
    val (i0, i1) = cut_array(images, howmany)
    val (l0, l1) = cut_array(labels, howmany)
    (MnistRawData(i0, l0), MnistRawData(i1, l1))
  }
}
final case class MnistRawDataSet(training_data: MnistRawData, validation_data: MnistRawData, test_data: MnistRawData)

final case class MnistRecord1(image: Matrix, label: Matrix)
final case class MnistRecord2(image: Matrix, label: Int) {
  def save() = {

    import java.awt.Point
    import java.awt.image._
    import javax.imageio.ImageIO
    val reshaped = image.reshape(28, 28)
    val pixels = reshaped.toArray()
    val (w, h) = reshaped.dim
    val raster = Raster.createWritableRaster(new PixelInterleavedSampleModel(0, w, h, 1, 1920, Array(0)), new Point(0, 0))

    for (i <- w.range; j <- h.range) { raster.setSample(i, j, 0, pixels(i)(j)) }
    val jimage = new BufferedImage(w, h, BufferedImage.TYPE_BYTE_GRAY);
    jimage.setData(raster)
    val output = new File(label + "." + System.currentTimeMillis() + ".jpg")
    ImageIO.write(jimage, "jpg", output)
  }
}
final case class MnistDataSet(training_data: Array[MnistRecord1], validation_data: Array[MnistRecord2], test_data: Array[MnistRecord2])
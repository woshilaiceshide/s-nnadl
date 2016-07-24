package woshilaiceshide.nnadl

import net.razorvine.pyro._
import net.razorvine.pickle.Unpickler
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import org.nd4j.linalg.factory.Nd4j

object MnistLoader {

  def using[T <: { def close(): Unit }, U](generator: => T)(op: T => U): U = {
    val empty = null.asInstanceOf[T]
    var resource: T = empty
    try {
      resource = generator
      op(resource)
    } finally {
      if (null != resource) { resource.close(); resource = empty }
    }
  }

  //TODO 
  def load_data() = {

    val path = s"""./data/mnist.pkl.gz"""

    using(new FileInputStream(path)) { input =>
      using(new GZIPInputStream(input)) { gzip =>
        val tmp = new Unpickler()
        tmp.load(gzip)
      }
    }

  }

}
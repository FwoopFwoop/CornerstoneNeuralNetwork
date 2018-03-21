package scala

import com.jmatio.io.MatFileReader
import com.jmatio.types.{MLArray, MLDouble, MLInt32}

class BabylonData {
  //Returns an array of 1797 input vector arrays
  def getTrainingImages(): Option[Array[Array[Double]]] = {
    readFile("mat/image_arr.mat") match {
      case Some(array) => {
        try {
          val dArray = array.asInstanceOf[MLDouble]
          var images = new Array[Array[Double]](1797)

          for (i <- 0 until 1797) {
            var image = new Array[Double](16)

            for (row <- 0 until 8) {
              for (col <- 0 until 8) {
                image(row + 8 * col) = dArray.get(i, row, col)
              }
            }

            images(i) = image
          }

          Some(images)
        } catch {
          case _ => None
        }

      }
      case None => None
    }
  }

  //Returns an array of 1797 input labels
  def getTrainingLabels(): Option[Array[Int]] = {
    readFile("mat/target.mat") match {
      case Some(array) => {
        try {
          val iArray = array.asInstanceOf[MLInt32]
          var labels = new Array[Int](1797)

          for(index <- 0 until 1797){
            labels(index) = iArray.get(index)
          }

          Some(labels)
        }catch{
          case _ => None
        }
      }
      case None => None
    }
  }


  private def readFile(filename: String): Option[MLArray] = {
    try {
      Some(new MatFileReader(filename).getMLArray("in"))
    } catch {
      case _ => None
    }
  }
}
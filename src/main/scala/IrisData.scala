class IrisData extends DataSource {
  val labelMap = Map("Iris-setosa" -> 0, "Iris-versicolor" -> 1, "Iris-virginica" -> 2)
  val nameMap = Map(0 -> "Setosa", 1 -> "Versicolor", 2 -> "Verginica")

  override def getOutputWidth(): Int = 3

  override def getInputWidth(): Int = 4

  def rawData() = scala.io.Source
          .fromFile("C:\\Users\\zacha\\OneDrive - Northeastern University\\GE 1502\\NeuralNetwork\\data\\Fisher Iris\\iris.data.txt").getLines().map(_.split(","))

  override def getData(): Array[Array[Double]] = {
    def removeLast[T](arr: Array[T]): Array[T] = {
      arr.reverse.tail.reverse
    }

    rawData().map(removeLast(_).map(_.toDouble)).toArray
  }

  override def getLabels(): Array[Int] = {
    (for (label <- rawData().map(_.last)) yield labelMap(label)).toArray
  }

  //Will return "" if the label is not valid
  override def getName(label: Int): String = {
    if (nameMap.keySet.contains(label)) {
      nameMap(label)
    } else {
      ""
    }
  }
}

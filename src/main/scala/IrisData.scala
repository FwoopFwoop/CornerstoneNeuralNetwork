import scala.collection.mutable

class IrisData extends DataSource {
  val labelMap = Map("Iris-setosa" -> 0, "Iris-versicolor" -> 1, "Iris-virginica" -> 2)
  val nameMap = Map(0 -> "Setosa", 1 -> "Versicolor", 2 -> "Verginica")

  override def getOutputWidth(): Int = 3

  override def getInputWidth(): Int = 4

  def rawData() = scala.io.Source
          .fromFile("C:\\Users\\zacha\\OneDrive - Northeastern University\\GE 1502\\NeuralNetwork\\data\\Fisher Iris\\iris.data.txt").getLines().map(_.split(","))

  def getInputs(): Array[Array[Double]] = {
    def removeLast[T](arr: Array[T]): Array[T] = {
      arr.reverse.tail.reverse
    }

    rawData().map(removeLast(_).map(_.toDouble)).toArray
  }

  def getLabels(): Array[Int] = {
    (for (label <- rawData().map(_.last)) yield labelMap(label)).toArray
  }

  def filterTest[T](array: Array[T]): mutable.ArraySeq[T] = {
    array.zipWithIndex.filter(_._2 % 4 == 0).map(_._1)
  }

  def filterTrain[T](array: Array[T]): mutable.ArraySeq[T] = {
    array.zipWithIndex.filter(_._2 % 4 != 0).map(_._1)
  }

  override def getTrainingData(): Array[Array[Double]] = {
    filterTrain(getInputs()).toArray
  }

  override def getTrainingLabels(): Array[Int] = {
    filterTrain(getLabels()).toArray
  }

  override def getTestData(): Array[Array[Double]] = {
    filterTest(getInputs()).toArray
  }

  override def getTestLabels(): Array[Int] = {
    filterTest(getLabels()).toArray
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

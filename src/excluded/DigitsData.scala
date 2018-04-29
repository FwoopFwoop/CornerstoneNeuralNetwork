object DigitsData extends DataSource {
  override def getInputWidth(): Int = 784

  override def getOutputWidth(): Int = 10

  val testData: (Array[Int], Array[Array[Double]]) = (Array(), Array())
    //loadData("out/production/NeuralNetwork/mnist_test.csv")
  val trainData: (Array[Int], Array[Array[Double]]) = (Array(), Array())
    //loadData("out/production/NeuralNetwork/mnist_train.csv")

  def loadData(fileName: String): (Array[Int], Array[Array[Double]]) = {
    val data = scala.io.Source.fromFile(fileName).getLines().map(_.split(",")).toArray
    println("data loaded")
    (data.map(_.head.toInt), data.map(_.tail.map(_.toDouble)))
  }

  override def getTrainingData(): Array[Array[Double]] = {
    trainData._2
  }

  override def getTrainingLabels(): Array[Int] = {
    trainData._1
  }

  override def getTestData(): Array[Array[Double]] = {
    testData._2
  }

  override def getTestLabels(): Array[Int] = {
    testData._1
  }

  override def getName(label: Int): String = label.toString

}

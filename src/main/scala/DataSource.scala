trait DataSource {
  def getInputWidth(): Int

  def getOutputWidth(): Int

  def getTrainingData(): Array[Array[Double]]

  def getTrainingLabels(): Array[Int]

  def getTestData(): Array[Array[Double]]

  def getTestLabels(): Array[Int]

  def getName(label: Int): String
}
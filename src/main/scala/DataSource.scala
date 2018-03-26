trait DataSource {
  def getInputWidth(): Int

  def getOutputWidth(): Int

  def getData(): Array[Array[Double]]

  def getLabels(): Array[Int]

  def getName(label: Int): String
}
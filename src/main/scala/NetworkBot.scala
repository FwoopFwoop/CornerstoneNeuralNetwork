class NetworkBot(val data: DataSource, val net: Network) {
  def this(dataSource: DataSource, layers: Int, neurons: Int, epochs: Int, learn:Double,
           adapt:Double) = {
    this(dataSource,
      new Network(layers, neurons, dataSource.getInputWidth(), dataSource.getOutputWidth(),
        learn, adapt))

    net.train(data.getTrainingData(), data.getTrainingLabels(), epochs)

  }

  def getAccuracy(): Double = net.test(data.getTestData(), data.getTestLabels())

}
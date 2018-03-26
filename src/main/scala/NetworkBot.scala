class NetworkBot(val data: DataSource, val net: Network) {
  def this(dataSource: DataSource, layers: Int, neurons: Int, epochs: Int) = {
    this(dataSource,
      new Network(layers, neurons, dataSource.getInputWidth(), dataSource.getOutputWidth()))

    net.train(data.getData(), data.getLabels(), epochs)

  }

  def getAccuracy(): Double = net.test(data.getData(), data.getLabels())

}
import scala.util.Random

class NetworkBot(val data: DataSource, val net: Network, val epochs: Int,
                 var trained: Boolean) {

  def this(dataSource: DataSource, layers: Int, neurons: Int, epochs: Int, learn: Double,
           adapt: Double) = {

    this(dataSource,
      new Network(layers, neurons, dataSource.getInputWidth(), dataSource.getOutputWidth(),
        learn, adapt), epochs, false)
  }

  def train(): Unit = {
    if (!trained) {
      trained = true
      net.train(data.getTrainingData(), data.getTrainingLabels(), epochs)
    }
  }

  def getAccuracy(): Double = net.test(data.getTestData(), data.getTestLabels())

  def getFitness(): Double = {

    val start = System.currentTimeMillis()

    val accuracy = getAccuracy()

    val runTime = System.currentTimeMillis() - start

    //sigmoid(accuracy) * 5 / sigmoid(runTime)

    accuracy
  }

  def produceOffspring(mate: NetworkBot): Vector[NetworkBot] = {
    val genes = (for ((c1, c2) <- this.getChromosomes() zip mate.getChromosomes())
      yield crossover(c1, c2)).unzip

    Vector(botFromGenes(genes._1), botFromGenes(genes._2))
  }

  def crossover(chromasome1: String, chromasome2: String): (String, String) = {
    if (chromasome1.length != chromasome2.length) {
      throw new RuntimeException("Cannot pair non-corresponding chromasomes")
    } else {
      val rand = new Random(System.currentTimeMillis())
      val splitIndex = rand.nextInt(chromasome1.length)

      val ans = (chromasome1.substring(0, splitIndex) + chromasome2.substring(splitIndex),
              chromasome2.substring(0, splitIndex) + chromasome1.substring(splitIndex))

      if (!ans._1.contains('1') || !ans._2.contains('1')) {
        crossover(mutate(ans._1), mutate(ans._2))
      } else ans
    }
  }

  def mutate(chromasome: String): String = {
    val rand = new Random(System.currentTimeMillis())
    val index = rand.nextInt(chromasome.length)

    chromasome.substring(0, index) + "1" + chromasome.substring(index + 1)
  }

  def botFromGenes(genes: Vector[String]): NetworkBot = {
    genes match {
      case Vector(layers, neurons, epochs, rate, adaptive) => {
        new NetworkBot(this.data, fromBinary(layers), fromBinary(neurons), fromBinary(epochs),
          fromBinary(rate).toDouble / 10.0, fromBinary(adaptive).toDouble / 10.0)
      }
    }
  }

  def getChromosomes(): Vector[String] = {
    Vector(toBinary(net.hiddenLayers.length, 2), toBinary(net.hiddenLayers.head.length, 4),
      toBinary(epochs, 13), toBinary((net.initRate * 10).toInt, 2),
      toBinary((net.adaptiveRate * 10).toInt, 2))
  }

  def toBinary(value: Int, length: Int): String = {
    if (length < 0) ""
    else if (value == 0) toBinary(value, length - 1) + "0"
    else toBinary(value / 2, length - 1) + (value % 2).toString
  }

  def fromBinary(value: String, place: Int = 1): Int = {
    if (value.equals("")) {
      0
    } else {
      value.last.toString.toInt * place + fromBinary(value.substring(0, value.length - 1), place * 2)
    }
  }

}
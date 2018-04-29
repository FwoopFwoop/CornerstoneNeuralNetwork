import scala.util.Random

class Network(var hiddenLayers: Array[Array[Neuron]], var outputLayer: Array[Neuron],
              inputWidth: Int, val initRate: Double, val adaptiveRate: Double) {

  if (initRate <= 0 || initRate > 1) {
    throw new IllegalArgumentException("Initial rate must be (0, 1], found " + initRate)
  }

  if (adaptiveRate < 0) {
    throw new IllegalArgumentException("Adaptive rate cannot be negative")
  }

  if (inputWidth <= 0) {
    throw new IllegalArgumentException("Input vectors must have a positive length")
  }


  //Initializes a network with the specified number of hidden layers + input/output width
  def this(layers: Int, neurons: Int, inputWidth: Int, outputWidth: Int, rate: Double,
           adapt: Double) {
    this(null, null, inputWidth, rate, adapt)

    hiddenLayers = createLayer(neurons, inputWidth) +:
            (1 until layers).map(_ => this.createLayer(neurons, neurons)).toArray

    outputLayer = createLayer(outputWidth, neurons)

    if (layers <= 0) {
      throw new IllegalArgumentException("Cannot create a network with 0 or less layers")
    }

    if (neurons <= 0) {
      throw new IllegalArgumentException("Hidden layers must have a positive number of neurons")
    }

    if (outputWidth <= 0) {
      throw new IllegalArgumentException("Output layer must have a positive number of neurons")
    }


    println("Network created with " + layers + " layers and " + neurons + " neurons")
  }

  //Creates a layer with random weights, with values initialized to 0
  def createLayer(neurons: Int, previous: Int): Array[Neuron] = {
    val rand = new Random(System.currentTimeMillis())

    (1 to neurons)
            .map(_ => new Neuron((1 to previous)
                    .map(_ => rand.nextDouble() * 2 - 1)
                    .toArray, 0))
            .toArray
  }

  //Modifies the weights of the network according to training data
  def train(data: Array[Array[Double]], labels: Array[Int], epochs: Int): Unit = {
    for (epoch <- 0 to epochs) {

      if (epoch % 100 == 0) println("Entering epoch " + epoch)

      for ((input, label) <- data zip labels) {

        propagate(input)

        var target = Array.fill(this.outputLayer.size)(.2)
        target(label) = .8

        val learnRate = initRate * math.exp(-adaptiveRate * epoch)

        backPropagate(target, input, learnRate)
      }
    }
  }

  //Back-propagate the error
  def backPropagate(target: Array[Double], input: Array[Double], learnRate: Double): Unit = {
    //Calculate responsibility for each output neuron in the error
    val outputResp = (outputLayer.map(_.value) zip target).map {
      case (o, t) => o * (1 - o) * (t - o)
    }

    //Update output weights
    for ((neuron, resp) <- outputLayer zip outputResp) {
      neuron.updateWeights(hiddenLayers.last.map(_.value * learnRate * resp))
    }

    //Initialize array to store
    var hiddenResp = new Array[Array[Double]](hiddenLayers.size)

    //Back Propagate hidden layers
    for (layer <- (0 until hiddenLayers.size).reverse) {

      //Calculate responsibility based on layer below

      //Allocate array size
      hiddenResp(layer) = new Array[Double](hiddenLayers(layer).size)

      //Calculate specific responsibilities
      for (index <- 0 until hiddenLayers(layer).size) {
        val h = hiddenLayers(layer)(index).value

        //If the layer is the last, it looks to the output layer,
        //otherwise the hidden layer below
        val previousInfo = {
          if (layer == hiddenLayers.size - 1) {
            outputLayer zip outputResp
          } else {
            hiddenLayers(layer + 1) zip hiddenResp(layer + 1)
          }
        }

        //Perform responsibility computation
        hiddenResp(layer)(index) = h * (1 - h) *
                previousInfo.map { case (n, r) => r * n.weights(index) }.sum

      }

      //Calculate new weights (referencing layer above)
      //Will use the input layer if this is the top hidden layer

      //For each neuron, weight pair
      for ((neuron, resp) <- hiddenLayers(layer) zip hiddenResp(layer)) {
        //Update the neurons weights
        neuron.updateWeights({
          if (layer == 0) {
            input
          } else {
            hiddenLayers(layer - 1).map(_.value)
          }
        } //multiply each value of the previous row by responsibility and learn rate
                .map(_ * resp * learnRate))
      }
    }

    //By this point, all weights should have been updated accordingly
  }

  //Passes the specified input into the network
  //Modifies the values of each neuron accordingly
  //Throws an exception if the input vector is the wrong size
  def propagate(input: Array[Double]): Unit = {

    //Pass the input into the first hidden layer
    hiddenLayers.head.foreach(_.evaluate(input))

    //For the rest of the hidden layers, evaluate based on the results of the previous layer
    for (i <- 1 until hiddenLayers.size) {
      hiddenLayers(i).foreach(_.evaluate(hiddenLayers(i - 1).map(_.value)))
    }

    //Evaluate the output layer based on the last hidden layer
    outputLayer.foreach(_.evaluate(hiddenLayers.last.map(_.value)))

  }

  //Returns what the network believes to be the correct label for the given input
  def classify(input: Array[Double]): Int = {
    propagate(input)

    outputLayer.map(_.value).indexOf(outputLayer.map(_.value).max)
  }

  //Returns how many of the test samples the network identifies correctly
  def test(data: Array[Array[Double]], labels: Array[Int]): Double = {
    var right: Double = 0
    var wrong: Double = 0

    for ((input, label) <- data zip labels) {
      if (classify(input) == label) {
        right += 1
      } else {
        wrong += 1
      }
    }

    right / (wrong + right)
  }

  //Calculates the mean square error of the output given the speified target
  //Throws an exception if the target is not the same size as the output vector
  def meanSquareError(target: Array[Double]): Double = {
    if (outputLayer.size != target.size) {
      throw new IllegalArgumentException("Target vector must be same length as output layer")
    }

    (outputLayer.map(_.value) zip target).map { case (o, t) => math.pow(t - o, 2) }.sum / target.size
  }
}
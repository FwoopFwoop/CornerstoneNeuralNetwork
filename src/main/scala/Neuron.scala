class Neuron(var weights: Array[Double], var value: Double) {
  //Evaluates the value of this neuron given the previous layer
  //Throws an exception if the input length is not equal to the number of weights
  def evaluate(inputs: Array[Double]): Unit = {
    if (inputs.size != weights.size) {
      throw new IllegalArgumentException("Wrong size input for this neuron")
    }

    this.value = sigmoid((inputs zip weights).map { case (i, w) => i * w }.sum)
  }

  //Applies the sigmoid function
  def sigmoid(x: Double): Double = 1 / (1 + math.exp(-x))

  //Applies the given changes to the weights of this node
  def updateWeights(deltas: Array[Double]): Unit = {
    weights = (weights zip deltas).map{case(w, d) => w + d}
  }
}
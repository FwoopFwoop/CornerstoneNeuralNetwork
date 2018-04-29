object BreedController {
  def main(args: Array[String]): Unit = {
    val numSpecimen = 102
    val startTime = System.currentTimeMillis()

    println(s"Running $numSpecimen specimen until stagnation")

    val breeder = new Breeder(102, new IrisData)

    val result = breeder.run(new UntilStagnation(new NetworkBot(new IrisData, 1, 1, 1, 1.0, 1.0)))

    val runtime:Double = (System.currentTimeMillis() - startTime)/1000.0

    println("Evolutions completed.")
    println(s"Runtime was $runtime seconds")
    println()
    println("Accuracy: " + result.getAccuracy())
    println("Epochs: " + result.epochs)
    println("Hidden Layers: " + result.net.hiddenLayers.size)
    println("Neurons per Layer: " + result.net.hiddenLayers.head.size)
    println("Learning rate: " + result.net.initRate)
    println("Adaptive Constant: " + result.net.adaptiveRate)

  }
}
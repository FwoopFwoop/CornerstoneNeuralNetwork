object NetworkController {
  def main(args: Array[String]): Unit = {
    val layers = 1
    val neurons = 16
    val epochs = 1822
    val learnRate = .0639
    val adaptiveConstant = 0.0143

    val startTime = System.currentTimeMillis()

    val bot = new NetworkBot(new IrisData, layers, neurons, epochs, learnRate, adaptiveConstant)

    bot.train()

    println("Training completed.")
    println("Accuracy against test data: " + bot.getAccuracy())

    val runtime:Double = (System.currentTimeMillis() - startTime) / 1000.0

    println(s"Runtime: $runtime seconds")
  }
}
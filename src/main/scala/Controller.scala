object Controller {
  def main(args: Array[String]): Unit = {
    val irisBot = new NetworkBot(new IrisData, 2, 10, 6000, .6, 0)

    println(irisBot.getAccuracy())
  }
}
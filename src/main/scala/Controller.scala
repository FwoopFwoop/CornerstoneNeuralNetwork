object Controller {
  def main(args:Array[String]):Unit ={
    val irisBot = new NetworkBot(new IrisData, 2, 11, 10000, .6, 0)

    println(irisBot.getAccuracy())
  }
}
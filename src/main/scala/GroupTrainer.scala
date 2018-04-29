class GroupTrainer(val subPop: Vector[NetworkBot]) extends Runnable{
  override def run(): Unit = {
    println("Starting thread to train " + subPop.size + " networks.")
    subPop.foreach(_.train)
  }
}
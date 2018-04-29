sealed abstract class TerminationCondition

case class FiniteIterations(iterations: Int) extends TerminationCondition

case class UntilStagnation(lastBest: NetworkBot, generation: Int) extends TerminationCondition{
  def this(lastBest: NetworkBot) = this(lastBest, 0)
}
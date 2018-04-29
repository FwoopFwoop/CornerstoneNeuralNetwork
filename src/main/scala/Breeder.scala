import java.util.concurrent.{Executors, TimeUnit}

import scala.util.Random

class Breeder(val size: Int, val dataSource: DataSource) {
  val THREADS = 6

  def run(condition: TerminationCondition, population: Vector[NetworkBot] = createInitialPopulation()): NetworkBot = {
    val survivors = cull(trainPopulation(population), (size * .75).toInt)
    val nextGen = mate(survivors) ++ survivors.slice(0, (size * .25).toInt)
    val max = nextGen.maxBy(_.getAccuracy())

    condition match {
      case FiniteIterations(n) =>{
        if(n <= 0){
          max
        }else{
          println(n-1 + " generations remaining")

          run(new FiniteIterations(n - 1), nextGen)
        }
      }
      case UntilStagnation(lastBest, generation) => {
        println(s"Generation $generation completed")
        if(max.getAccuracy() <= lastBest.getAccuracy()){
          lastBest
        }else{
          run(new UntilStagnation(max, generation + 1), nextGen)
        }
      }
    }
  }

  def createInitialPopulation(): Vector[NetworkBot] = {
    val rand = new Random()

    (0 to size).map(_ =>
      new NetworkBot(dataSource, rand.nextInt(4) + 1, rand.nextInt(16) + 1, rand.nextInt(4000) + 500,
        rand.nextDouble(), rand.nextDouble())).toVector
  }

  def trainPopulation(population: Vector[NetworkBot]): Vector[NetworkBot] = {
    val trainers = population.sliding(population.size / THREADS).toVector.map(new GroupTrainer(_))

    val pool = Executors.newFixedThreadPool(THREADS)

    trainers.foreach(pool.execute(_))
    pool.shutdown()
    pool.awaitTermination(10, TimeUnit.MINUTES)

    population
  }

  //Returns the survivors sorted by fitness
  def cull(population: Vector[NetworkBot], survivors: Int): Vector[NetworkBot] = {
    (for (i <- 0 until survivors) yield select(population))
            .toVector.sortWith(_.getFitness() > _.getFitness())
  }

  def select(population: Vector[NetworkBot]): NetworkBot = {
    val fitnessVector: Vector[Int] = population.map(_.getFitness() * 1000).map(_.toInt)

    val fitnessSum = fitnessVector.sum

    val selection = new Random().nextInt(fitnessSum)
    var sum = 0
    var index = -1

    while (sum < selection) {
      index += 1
      sum += fitnessVector(index)
    }

    population(index)
  }

  def mate(population: Vector[NetworkBot]): Vector[NetworkBot] = {
    (for (index <- 0 until population.size / 2) yield {
      population(index).produceOffspring(select(population))
    }).toVector.flatten
  }

}
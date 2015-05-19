package org.eldhuset.it3708

import org.eldhuset.it3708.flatland.Flatland
import org.eldhuset.it3708.qlearning.Agent
import java.io.File

import org.eldhuset.it3708.qlearning.ScenarioRepresentation

case class Config(scenario: File = null, steps: Int = 1000, delay: Int = 500)

object Main {
  val optionsParser = new scopt.OptionParser[Config]("qlearning") {

    opt[File]('s', "scenario") required() valueName("<file>") action {
      (s, c) => c.copy(scenario = s)
    } text("A text file containing the scenario definition.")

    opt[Int]('k', "steps") optional() valueName("<n>") action {
      (k, c) => c.copy(steps = k)
    } text(
      "The number of iterations for the Q Learning algorithm. (Default = 1000.)")

    opt[Int]('d', "delay") optional() valueName("<n>") action {
      (d, c) => c.copy(delay = d)
    } text(
      "The duration, in ms, of each state in the final visualization. " +
      "(Default = 500.)")
  }

  def main(args: Array[String]): Unit = {
    val config = optionsParser.parse(args, Config())

    config match {
      case Some(config) => {
        val flatland = Flatland.fromFile(config.scenario)
        val steps = config.steps

        prepareScreen()
        val t0 = System.currentTimeMillis()
        println(
          s"Running Q learning on ${config.scenario} with $steps iterations.")

        val q = qlearning.learnQ(scenario = flatland, steps = steps)

        val t1 = System.currentTimeMillis()
        val delta = math.round((t1 - t0) / 1000.0)
        println(s"Finished in approximately $delta s.")
        println("Press Enter to visualize the resulting agent.")
        scala.io.StdIn.readLine()

        val agent = Agent(scenario = flatland, q = q)
        val stats = agent run { (representation) =>
          prepareScreen()
          println(representation)
          Thread.sleep(config.delay)
        }

        println(s"\n$stats")
      }
      case None =>
        // Invalid configuration. Error message will be displayed by scopt.
    }
  }

  def clearScreen(): Unit = print("\u001b[2J")

  def moveCursorToTopOfScreen(): Unit = print("\u001b[H")

  def prepareScreen(): Unit = {
    clearScreen()
    moveCursorToTopOfScreen()
  }
}

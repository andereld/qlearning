package org.eldhuset.it3708

import org.eldhuset.it3708.flatland.{Flatland, FlatlandAction}
import java.io.File

case class Config(scenario: File = null)

object Main {
  val optionsParser = new scopt.OptionParser[Config]("QLearning") {
    head("QLearning")
    opt[File]('s', "scenario") required() valueName("<file>") action {
        (s, c) => c.copy(scenario = s)
      } text ("A text file containing the scenario definition.")
  }

  def main (args: Array[String]): Unit = {
    val config = optionsParser.parse(args, Config())

    config match {
      case Some(config) => {
        val flatland = Flatland.fromFile(config.scenario)
        val action = new FlatlandAction()
        println(flatland.update(action))
      }
      case None =>
        // Invalid configuration. Error message will be displayed by scopt.
    }
  }
}

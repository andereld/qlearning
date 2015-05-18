package org.eldhuset.it3708.qlearning

import org.eldhuset.it3708.flatland.{Flatland, State}

import scala.annotation.tailrec

case class Agent(scenario: Flatland, q: Q) {
  def run(callback: ScenarioRepresentation => Unit): Statistics = {
    val initialState = State.initialState(scenario)
    val initialStats = Statistics(totalFood =  scenario.initialFoodCount)

    runner(initialState, scenario, q, initialStats)(callback)
  }

  @tailrec
  private def runner(state: State, scenario: Flatland, q: Q, stats: Statistics)
                    (callback: ScenarioRepresentation => Unit)
                    : Statistics = {

    val representation = ScenarioRepresentation(
      scenario = scenario,
      state = state,
      q = q)

    callback(representation)

    if (state.finished(scenario)) {
      stats
    } else {
      val action = bestAction(state, q)
      val (consumedCell, newScenario) = action(state, scenario)
      val newState = state.updated(
        newPosition = action.coordinates(state, scenario),
        consumedCell = consumedCell)
      val newStats = stats.updated(consumedCell)

      runner(newState, newScenario, q, newStats)(callback)
    }
  }
}

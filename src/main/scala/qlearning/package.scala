package org.eldhuset.it3708

import org.eldhuset.it3708.flatland.{Action, Coordinates, Flatland, State}

import scala.annotation.tailrec
import scala.util.Random

package object qlearning {

  type ActionMap = Map[Action, Double]
  def ActionMap(mappings: (Action, Double)*): ActionMap =
    Map[Action, Double](mappings: _*)

  type Q = Map[State, ActionMap]
  def Q(mappings: (State, ActionMap)*): Q = Map[State, ActionMap](mappings: _*)

  type BackupList = List[(State, Action, Double)]
  def BackupList(): BackupList = List[(State, Action, Double)]()

  val backupSteps = 4

  def learnQ(scenario: Flatland, steps: Int): Q = {
    val beginningState = State.initialState(scenario)
    val beginningQ = Q(beginningState -> ActionMap())

    learn(scenario, beginningQ, beginningState, steps)
  }

  @tailrec
  def learn(scenario: Flatland, q: Q, beginningState: State, steps: Int,
      k: Int = 1): Q =
    k match {
      case n: Int if n == steps + 1 => q
      case n: Int if n > 0          => learn(
        scenario = scenario,
        q = solve(scenario, beginningState, q, k),
        beginningState = beginningState,
        steps = steps,
        k = k + 1)
    }

  @tailrec
  def solve(scenario: Flatland, state: State, q: Q, k: Int,
      backupList: BackupList = BackupList()): Q = {

    if (state.finished(scenario))
      return q

    val action = selectAction(state, q, k)
    val (consumedCell, newScenario) = action(state, scenario)
    val newState = state.updated(
      newPosition = action.coordinates(state, scenario),
      consumedCell = consumedCell)
    val reward = consumedCell.reward(scenario)
    val newQ = updateQ(q, state, newState, action, reward)
    val newBackupList = updateBackupList(state, action, reward, backupList)

    solve(newScenario, newState, newQ, k, newBackupList)
  }

  def updateBackupList(state: State, action: Action, reward: Double,
      list: BackupList): BackupList =
    ((state, action, reward) :: list) take backupSteps

  def bestAction(state: State, q: Q) = q get state match {
    case Some(actions) if actions.nonEmpty => (actions maxBy (_._2))._1
    case Some(actions) if actions.isEmpty  => Action.randomAction
    case None                              => Action.randomAction
  }

  def selectAction(state: State, q: Q, k: Int): Action =
    if (Random.nextDouble < 3.0 / math.log(k))
      Action.randomAction
    else
      bestAction(state, q)

  def valueForAction(action: Action, state: State, q: Q): Double = {
    val actions = q getOrElse(state, ActionMap())

    actions getOrElse(action, 0.0)
  }


  def updateQ(q: Q, oldState: State, newState: State, action: Action,
      reward: Double, step: Int = backupSteps)
      : Q = {

    val learningRate = 0.1
    val discountFactor = 0.5

    val calculatedReward = calculateReward(
      learningRate = learningRate,
      discount = discountFactor,
      immediateReward = reward,
      oldState = oldState,
      action = action,
      newState = newState,
      q = q)

    val oldActionMapping = q getOrElse(oldState, ActionMap())
    val newActionMapping = oldActionMapping + (action -> calculatedReward)
    val newQ = q + (oldState -> newActionMapping)

    newQ
  }

  def calculateReward(learningRate: Double, discount: Double,
      immediateReward: Double, oldState: State, action: Action, newState: State,
      q: Q): Double = {

    val oldValue = valueForAction(action, oldState, q)
    val futureEstimate =
      (Action.possibleActions map (valueForAction(_, newState, q))).max
    val learnedValue = immediateReward + discount * futureEstimate

    oldValue + learningRate * (learnedValue - oldValue)
  }
}

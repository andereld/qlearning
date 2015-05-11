package org.eldhuset.it3708.qlearning

trait Action[A <: Scenario[A]] {
  def apply(scenario: A): A
}

trait Scenario[A <: Scenario[A]] { this: A =>
  def update(action: Action[A]): A = action(this)
}

object QLearning {
  def qLearning(steps: Int = 10): Unit = {
  }
}

package org.eldhuset.it3708.flatland

import org.eldhuset.it3708.qlearning

class FlatlandAction extends qlearning.Action[Flatland] {
  def apply(scenario: Flatland): Flatland = scenario
}

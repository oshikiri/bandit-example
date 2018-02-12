package org.oshikiri.example.bandit

import breeze.stats.distributions._

import org.oshikiri.example.bandit.SimpleBanditAlgorithm._
import org.oshikiri.example.bandit.SimpleBanditAlgorithm.Types._

class BernoulliSlotMachine(override val armsWithExpectedRewards: Map[SimpleBanditArm, Reward],
                           override val seed: Int)
    extends SimpleSlotMachine(armsWithExpectedRewards, seed) {
  import BetaBernoulliTS.BetaBernoulliBanditArm

  def drawReward(arm: SimpleBanditArm,
                 expectedReward: Reward)(implicit randBasis: RandBasis): Reward = arm match {
    case bbbArm: BetaBernoulliBanditArm =>
      drawReward(bbbArm, expectedReward)
    case other =>
      sys.error(s"Invalid type of arm: s{other}")
  }

  private def drawReward(arm: BetaBernoulliBanditArm,
                         expectedReward: Reward)(implicit randBasis: RandBasis): Reward = {
    val distribution = new Bernoulli(p = expectedReward)(randBasis)
    if (distribution.draw()) 1.0 else 0.0
  }
}

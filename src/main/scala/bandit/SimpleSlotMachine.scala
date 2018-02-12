package org.oshikiri.example.bandit

import breeze.stats.distributions.RandBasis
import org.oshikiri.example.bandit.SimpleBanditAlgorithm.SimpleBanditArm
import org.oshikiri.example.bandit.SimpleBanditAlgorithm.Types._

// TODO: 途中で腕の本数が変わるケース?
abstract class SimpleSlotMachine(val stateWithExpectedRewards: Map[SimpleBanditArm, Reward],
                                 val seed: Int) {
  implicit val randBasis: RandBasis = RandBasis.withSeed(seed)

  def drawReward(arm: SimpleBanditArm, expectedReward: Reward)(
    implicit randBasis: RandBasis
  ): Reward

  def pullArm(armId: ArmId): Reward = {
    stateWithExpectedRewards
      .filter {
        case (arm: SimpleBanditArm, expectedReward: Reward) =>
          arm.armId == armId
      }
      .map {
        case (arm: SimpleBanditArm, expectedReward: Reward) =>
          drawReward(arm, expectedReward)
      }
      .headOption
      .getOrElse(0.0)
  }
}

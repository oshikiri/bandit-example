package org.oshikiri.example.bandit

import math.{log, sqrt}

import breeze.stats.distributions._

import org.oshikiri.example.bandit.SimpleBanditAlgorithm._
import org.oshikiri.example.bandit.SimpleBanditAlgorithm.Types._

class LognormalSlotMachine(
  override val stateWithExpectedRewards: Map[SimpleBanditArm, Reward],
  override val seed: Int,
  val trueVarianceOfLogRewards: Double
) extends SimpleSlotMachine(stateWithExpectedRewards, seed) {
  import LognormalTS.LognormalBanditArm

  def drawReward(arm: SimpleBanditArm, expectedReward: Reward)(
    implicit randBasis: RandBasis
  ): Reward = arm match {
    case lArm: LognormalBanditArm =>
      drawReward(lArm, expectedReward, trueVarianceOfLogRewards)
    case other =>
      sys.error(s"Invalid type of arm: s{other}")
  }

  private def drawReward(
    arm: LognormalBanditArm,
    expectedReward: Reward,
    trueVarianceOfLn: Double
  )(implicit randBasis: RandBasis): Reward = {
    val mu = log(expectedReward) - trueVarianceOfLn / 2.0
    val sigma = sqrt(trueVarianceOfLn)
    val distribution = new LogNormal(mu, sigma)(randBasis)
    distribution.draw()
  }
}

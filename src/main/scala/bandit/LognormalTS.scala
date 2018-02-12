package org.oshikiri.example.bandit

import math.{ pow, sqrt, log, exp }

import scala.collection.immutable.Seq
import breeze.stats.distributions._

import org.oshikiri.example.bandit.SimpleBanditAlgorithm._
import org.oshikiri.example.bandit.SimpleBanditAlgorithm.Types._


class LognormalTS extends SimpleBanditAlgorithm {
  import LognormalTS._

  def estimatedRewards(state: LognormalBanditState): Map[ArmId, Reward] =
    sampleTheta(state)

  def updateState(
    state: LognormalBanditState,
    choosedArmId: ArmId,
    reward: Reward
  ) = LognormalBanditState(
    nTrials = state.nTrials + 1,
    arms = updateArms(state.arms, choosedArmId, reward)
  )

  private def updateArms(
    arms: Seq[LognormalBanditArm],
    choosedArmId: ArmId,
    reward: Reward
  ) = arms.map {
    case arm if arm.armId == choosedArmId =>
      val logReward = log(reward)
      val (muNew, sigmaNew) = updateParameters(
        arm.distribution.mu, arm.distribution.sigma, arm.sampleVarianceOfLogRewards, reward)

      arm.copy(
        nChoosed = arm.nChoosed + 1,
        sumOfRewards = arm.sumOfRewards + reward,
        sumOfLogRewards = arm.sumOfLogRewards + logReward,
        sumOfSquaredLogRewards = arm.sumOfSquaredLogRewards + logReward * logReward,
        distribution =
          if (arm.nChoosed < 10) arm.distribution // FIXME: workaround
          else LognormalDistribution(muNew, sigmaNew)
      )
    case otherArm => otherArm
  }

  private def updateParameters(
    muOld: Double,
    sigmaOld: Double,
    sampleVarianceOfLogRewards: Double,
    reward: Reward
  ): (Double, Double) = {
    val sigmaSquaredOld = pow(sigmaOld, 2)
    val sigmaSquaredNew =
      sigmaSquaredOld * sampleVarianceOfLogRewards / (sigmaSquaredOld + sampleVarianceOfLogRewards)
    val muNew = sigmaSquaredNew * (
      muOld / sigmaSquaredOld + log(reward) / sampleVarianceOfLogRewards + 0.5)

    val sanitizedMuNew = sanitizeMu(muNew, muOld)
    val sanitizedSigmaSquaredNew = sanitizeSigmaSquared(sigmaSquaredNew, sigmaSquaredOld)

    (sanitizedMuNew, sqrt(sanitizedSigmaSquaredNew))
  }

  private def sanitizeMu(mu: Double, default: Double) =
    if(mu.isNaN || mu.isInfinity) default
    else mu

  private def sanitizeSigmaSquared(sigmaSquared: Double, default: Double) =
    if (sigmaSquared.isNaN || sigmaSquared.isInfinity || sigmaSquared <= 0) default
    else sigmaSquared
}

object LognormalTS extends SimpleBanditAlgorithm {

  def initialStateFlatPrior(nArms: Int) = LognormalBanditState(
    arms = (0 until nArms).map { armId =>
      LognormalBanditArm(
        armId = armId.toLong,
        nChoosed = 0L,
        sumOfRewards = 0.0,
        sumOfLogRewards = 0.0,
        sumOfSquaredLogRewards = 0.0,
        distribution = LognormalDistribution(mu = 30.0, sigma = 100.0)
      )
    },
    nTrials = 0
  )

  case class LognormalBanditState(arms: Seq[LognormalBanditArm], nTrials: Long) extends SimpleBanditState

  case class LognormalBanditArm(
    armId: ArmId,
    nChoosed: Long,
    sumOfRewards: Double,
    sumOfLogRewards: Double,
    sumOfSquaredLogRewards: Double,
    distribution: LognormalDistribution
  ) extends SimpleBanditArm {
    lazy val meanLogRewards = sumOfLogRewards / nChoosed.toDouble
    lazy val meanSquaredLogRewards = sumOfSquaredLogRewards / nChoosed.toDouble
    lazy val sampleVarianceOfLogRewards = meanSquaredLogRewards - math.pow(meanLogRewards, 2)
  }
}


class LognormalSlotMachine(
  override val stateWithExpectedRewards: Map[SimpleBanditArm, Reward],
  override val seed: Int,
  val trueVarianceOfLogRewards: Double
) extends SimpleSlotMachine(stateWithExpectedRewards, seed) {
  import LognormalTS.LognormalBanditArm

  def drawReward(
    arm: SimpleBanditArm,
    expectedReward: Reward
  )(implicit randBasis: RandBasis): Reward = arm match {
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

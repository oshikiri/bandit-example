package org.oshikiri.example.bandit

import scala.math.pow
import scala.collection.immutable.Seq
import breeze.stats.distributions._

import org.oshikiri.example.bandit.SimpleBanditAlgorithm._
import org.oshikiri.example.bandit.SimpleBanditAlgorithm.Types._

class BetaBernoulliTS extends SimpleBanditAlgorithm {
  import BetaBernoulliTS._

  def estimatedRewards(state: BetaBernoulliBanditState): Map[ArmId, Reward] =
    sampleTheta(state)

  def updateState(state: BetaBernoulliBanditState, choosedArmId: ArmId, reward: Reward) =
    BetaBernoulliBanditState(
      nTrials = state.nTrials + 1,
      arms = updateArms(state.arms, choosedArmId, reward)
    )

  private def updateArms(arms: Seq[BetaBernoulliBanditArm], choosedArmId: ArmId, reward: Reward) =
    arms.map {
      case arm if arm.armId == choosedArmId =>
        arm.copy(
          nChoosed = arm.nChoosed + 1,
          nSuccess = arm.nSuccess + reward.toLong,
          distribution = BetaDistribution(
            alpha = arm.distribution.alpha + reward,
            beta = arm.distribution.beta + (1.0 - reward)
          )
        )
      case otherArm => otherArm
    }
}

object BetaBernoulliTS extends SimpleBanditAlgorithm {
  def initialStateUniformPrior(nArms: Int) = BetaBernoulliBanditState(
    arms = (0 until nArms).map { armId =>
      BetaBernoulliBanditArm(
        armId = armId.toLong,
        nChoosed = 0L,
        nSuccess = 0L,
        distribution = BetaDistribution(alpha = 1, beta = 1)
      )
    },
    nTrials = 0
  )

  case class BetaBernoulliBanditState(arms: Seq[BetaBernoulliBanditArm], nTrials: Long)
      extends SimpleBanditState

  case class BetaBernoulliBanditArm(armId: ArmId,
                                    nChoosed: Long,
                                    nSuccess: Long,
                                    distribution: BetaDistribution)
      extends SimpleBanditArm
}

class BernoulliSlotMachine(override val stateWithExpectedRewards: Map[SimpleBanditArm, Reward],
                           override val seed: Int)
    extends SimpleSlotMachine(stateWithExpectedRewards, seed) {
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

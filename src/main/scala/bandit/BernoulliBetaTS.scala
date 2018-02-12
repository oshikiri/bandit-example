package org.oshikiri.example.bandit

import scala.collection.immutable.Seq

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

package org.oshikiri.example.bandit

abstract class SimpleBanditAlgorithm {
  import SimpleBanditAlgorithm._
  import SimpleBanditAlgorithm.Types._

  def chooseBestArm(armIdWithScores: Map[ArmId, Reward]): ArmId =
    armIdWithScores.maxBy(_._2)._1

  def sampleTheta(state: SimpleBanditState): Map[ArmId, Double] =
    state.arms.map { arm =>
      arm.armId -> arm.distribution.draw()
    }.toMap
}

object SimpleBanditAlgorithm {
  import org.oshikiri.example.bandit.ProbabilityDistribution

  object Types {
    type ArmId = Long
    type Reward = Double
  }

  abstract class SimpleBanditArm {
    val armId: Types.ArmId
    val nChoosed: Long
    val distribution: ProbabilityDistribution
  }

  abstract class SimpleBanditState {
    val arms: Seq[SimpleBanditArm]
  }
}

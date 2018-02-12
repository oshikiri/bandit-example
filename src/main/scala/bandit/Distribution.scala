package org.oshikiri.example.bandit

import breeze.stats.distributions.{Beta, LogNormal}

abstract class ProbabilityDistribution {
  val mean: Double
  val variance: Double
  def draw(): Double
}

case class BetaDistribution(alpha: Double, beta: Double) extends ProbabilityDistribution {
  private val distribution = new Beta(alpha, beta)

  lazy val mean: Double = distribution.mean
  lazy val variance: Double = distribution.variance

  def draw(): Double = distribution.draw()
}

case class LognormalDistribution(mu: Double, sigma: Double) extends ProbabilityDistribution {
  private val distribution = new LogNormal(mu, sigma)

  lazy val mean: Double = distribution.mean
  lazy val variance: Double = distribution.variance

  def draw(): Double = distribution.draw()
}

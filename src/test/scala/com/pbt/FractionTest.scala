package com.pbt

import org.scalatest.{Matchers, WordSpecLike}
import org.scalatest.prop.PropertyChecks

class FractionTest extends WordSpecLike with Matchers with PropertyChecks {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 100)

  "Sending allowed values to Fraction" should {

    "properly normalize" in {
      forAll { (n: Int, d: Int) =>
        whenever(d != 0 && d != Integer.MIN_VALUE && n != Integer.MIN_VALUE) {

          println(s"$n $d")

          val f = new Fraction(n, d)

          if (n < 0 && d < 0 || n > 0 && d > 0)
            f.numer should be > 0
          else if (n != 0)
            f.numer should be < 0
          else
            f.numer shouldEqual 0

          f.denom should be > 0
        }
      }
    }

  }

  "Sending not allowed values to Fraction" should {
    "fail with exception" in {
      val invalidCombos =
        Table(
          ("n", "d"),
          (Integer.MIN_VALUE, Integer.MIN_VALUE),
          (1, Integer.MIN_VALUE),
          (Integer.MIN_VALUE, 1),
          (Integer.MIN_VALUE, 0),
          (1, 0)
        )

      forAll(invalidCombos) { (n: Int, d: Int) =>
        an [IllegalArgumentException] should be thrownBy {
          new Fraction(n, d)
        }
      }
    }
  }

}

class Fraction(n: Int, d: Int) {

  require(d != 0)
  require(d != Integer.MIN_VALUE)
  require(n != Integer.MIN_VALUE)

  val numer = if (d < 0) -1 * n else n
  val denom = d.abs

  override def toString = numer + " / " + denom
}

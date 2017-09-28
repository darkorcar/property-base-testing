package com.pbt

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpecLike}

class ReverseListTest extends WordSpecLike with Matchers with PropertyChecks {

  "Reversing List" should {

    "prove symmetry" in {

      forAll { (list: List[Int]) =>
        list.reverse.reverse shouldBe list
      }
    }

    //Redundant
    "prove invariant size" in {

      forAll { (list: List[Int]) =>
        list.reverse.size shouldBe list.size
      }
    }

    "prove constains the same elements" in {

      forAll { (list: List[Int]) =>
        list.reverse.diff(list) shouldBe Nil

      }
    }

    "prove consistent last element becomes firsts" in {

      forAll { (list: List[Int]) =>
        whenever(list.nonEmpty) {
          list.reverse.head shouldBe list.last
        }
      }
    }

    "prove consistent first element becomes last" in {

      forAll { (list: List[Int]) =>
        whenever(list.nonEmpty) {
          list.reverse.last shouldBe list.head
        }
      }
    }

    "prove middle element it's invariant in a list with odd number of elements" in {

      forAll { (list: List[Int]) =>
        whenever(list.nonEmpty && list.size % 2 == 1) {
          val middle = list.size / 2
          list.reverse(middle) shouldBe list(middle)
        }
      }
    }

    "prove middle element it's invariant in a list with odd number of elements V2" in {

      forAll(oddListSize) { list =>
        val middle = list.size / 2
        list.reverse(middle) shouldBe list(middle)
      }
    }

  }

  val oddListSize: Gen[List[Int]] = Arbitrary.arbitrary[List[Int]] suchThat (_.size % 2 == 1)

}

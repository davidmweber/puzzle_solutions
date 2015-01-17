package co.horn.puzzle

import org.scalatest.{FunSpec, Matchers}
import co.horn.puzzle.PuzzleTime._

class PuzzleTimeTest extends FunSpec with Matchers {

  describe("Test puzzles can") {

    it("returns the last element in the list") {
      last(List("a", "b", "c")) should be("c")
      last(List(1)) should be(1)
    }

    it("returns the penultimate  element in a list") {
      penultimate(List("a", "b", "c")) should be("b")
      penultimate(List("a", "c")) should be("a")
    }

    it("returns the k'th element in a list") {
      kth_element(2, List(2, 3, 4, 5, 6)) should be(4)
      kth_element(0, List(2, 3, 4, 5, 6)) should be(2)
      kth_element(4, List(2, 3, 4, 5, 6)) should be(6)
    }

    it("counts the number of elements in a list") {
      count_elems(List(0, 1, 2)) should be(3)
      count_elems(List(0)) should be(1)
      count_elems(List()) should be(0)
    }

    it("reverses a list") {
      reverse(List(2, 3, 4, 5, 6)) should be(List(6, 5, 4, 3, 2))
      reverse(List(2)) should be(List(2))
      reverse(List()) should be(List())
    }

    it("detects palindromes") {
      isPalindrome(List(1, 2, 3, 4, 3, 2, 1)) should be(right = true)
      isPalindrome(List(1, 2, 3, 4, 3, 2)) should be(right = false)
      isPalindrome(List()) should be(right = true)
    }

    it("flattens lists") {
      flatten(List(List(1, 2, 3), 4, List(5, List(6, List(7, 8))))) should be(List(1, 2, 3, 4, 5, 6, 7, 8))
      flatten(List()) should be(List())
      flatten(List("a")) should be(List("a"))
      flatten(List("a", List(2, 3))) should be(List("a", 2, 3))
    }

    it("removes duplicates in a list") {
      compress(List(1, 2, 3, 3, 1)) should be(List(1, 2, 3, 1))
    }

    it("Separates consecutive duplicates into separate lists") {
      pack(List(1, 1, 1, 3, 5, 5, 7)) should be(List(List(1, 1, 1), List(3), List(5, 5), List(7)))
      pack(List()) should be(List())
    }

    it("run-length encodes a list") {
      encode(List('b, 'b, 'b, 'z, 'c, 'c, 'c, 'z, 'z)) should be(List((3, 'b), (1, 'z), (3, 'c), (2, 'z)))
    }

    it("produces modified run-length encoded data") {
      encodeMod(List('b, 'b, 'b, 'z, 'c, 'c, 'c, 'z, 'z)) should be(List((3, 'b), 'z, (3, 'c), (2, 'z)))
    }

    it("removes the k'th element of a list") {
      removeAt(4, List(0, 1, 2, 3, 4, 5, 6)) should be((List(0, 1, 2, 3, 5, 6), 4))
    }

    it("removes a random subset of N elements from a list") {
      val t = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      0 until 100 foreach { i ⇒
        1 until 10 foreach { j ⇒
          val s = randomSelect(j, t).toSet
          s subsetOf t.toSet should be(right = true)
          s.size should be (j)
        }
      }
    }

    it("randomly permutes a list") {
      val t = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      0 until 100 foreach { i ⇒
        randomPermute1(t).toSet should be(t.toSet)
      }
    }
  }
}

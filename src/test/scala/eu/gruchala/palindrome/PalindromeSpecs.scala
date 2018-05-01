package eu.gruchala.palindrome

import eu.gruchala.BaseSpec

import eu.gruchala.palindrome.Palindrome._

class PalindromeSpecs extends BaseSpec {

  val expected = Seq(
    "asd" -> false,
    "a" -> true,
    "aaaa" -> true,
    "aaa aaa" -> true,
    "aaa bbb" -> false,
    "madam" -> true,
    "racecar" -> true,
  )

  def check(palindrome: String => Boolean) = {
    expected.map { case (s, r) =>
      palindrome(s) shouldEqual r
    }
  }

  "mark correct words and phrases" when {
    "isPalindrome is invoked" in {
      check(isPalindrome)
    }

    "isPalindrome2 is invoked" in {
      check(isPalindrome2)
    }

    "isPalindromeFold is invoked" in {
      check(isPalindromeFold)
    }

    "isPalindromeReduce is invoked" in {
      check(isPalindromeReduce)
    }
  }
}

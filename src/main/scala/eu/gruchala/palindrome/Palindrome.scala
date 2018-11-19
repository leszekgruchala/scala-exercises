package eu.gruchala.palindrome

object Palindrome {

  def isPalindrome(s: String): Boolean = {
    (for (i <- 0 until s.length/2) yield {
      s.charAt(i) == s.charAt(s.length - 1 - i)
    }).forall(_ == true)
  }

  def isPalindrome2(s: String): Boolean = {
    val lastIndex = s.length - 1

    !(for (i <- 0 until lastIndex if i <= lastIndex/2) yield {
      s.charAt(i) == s.charAt(lastIndex - i)
    }).contains(false)
  }

  def isPalindromeFold(s: String): Boolean = {
    s.foldLeft("")((l, acc) => acc + l) == s
  }

  def isPalindromeReduce(s: String): Boolean = {
    (for (x <- 0 to s.length/2) yield s(x) == s(s.length - x - 1))
      .reduceLeft((acc,n)=> acc && n)
  }

  def groupPalindromes(str: String): Seq[String] =
    for {i <- 2 to str.length; s <- str.sliding(i) if s == s.reverse} yield s

}

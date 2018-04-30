package eu.gruchala.palindrome

object Palindrome {

  def isPalindrome(str: String): Boolean = {
    val lastIndex = str.length - 1

    !(for (i <- 0 until lastIndex if i <= lastIndex/2) yield {
      str.charAt(i) == str.charAt(lastIndex - i)
    }).contains(false)
  }

}

package exercises

object Palindrome {
  /** 
   *  True if the string is a palindrome, after removing all non-alphabetic 
   *  characters (e.g., spaces, numbers, and punctuation).
   *  
   *  Hint: the Scala collections API is your friend
   *  http://docs.scala-lang.org/overviews/collections/overview.html 
   */
  def isPalindrome(s: String): Boolean = {
  	val alphaChars = s.filter(_.isLetter);
  	val lowercaseChars = alphaChars.toLowerCase();
    palindromeHelper(lowercaseChars)
  }

   def palindromeHelper(s: String): Boolean = {
   	if (s.length <= 1) true
   	else
   		if(!(s(s.length - 1) == s(0))) return false
   		else (palindromeHelper(s.substring(1, s.length - 1)))
   }
}
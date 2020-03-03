package Utils

/**
  * Contains the functions necessary to calculate the number of *clinks* when n people want to cheers.
  */
object ClinksCalculator {
  /**
    * Calculate the factorial of a given number.
    * @param n the number to compute
    * @return n!
    */
  def factorial(n: Int): Int = {
    def loop(acc: Int, n: Int): Int = acc match {
      case x if x <= 0 => acc
      case _ => loop(acc*n, n-1)
    }

    loop(1, n)
  }

  /**
    * Calculate the combination of two given numbers.
    * @param n the first number
    * @param k the second number
    * @return n choose k
    */
  def calculateCombination(n: Int, k: Int): Int =
    // Combination formula = n! / (k! * (n - k)!)
    factorial(n) / (factorial(k) * factorial(n - k))
}

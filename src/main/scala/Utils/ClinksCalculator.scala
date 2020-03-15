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
    def loop(x: Int, acc: Int): Int = x match {
      case 0 => acc
      case _ => loop(x-1, x*acc)
    }

    loop(n, 1)
  }

  /**
    * Calculate the combination of two given numbers.
    * @param n the first number
    * @param k the second number
    * @return n choose k
    */
  def calculateCombination(n: Int, k: Int): Int = {
    factorial(n) / (factorial(k) * factorial(n-k))
  }
}

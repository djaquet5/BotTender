package Utils

import java.lang

import Dictionary.dictionary

object SpellChecker {
  /**
    * Calculate the Levenshtein distance between two words.
    * @param s1 the first word
    * @param s2 the second word
    * @return an integer value, which indicates the Levenshtein distance between "s1" and "s2"
    */
  // TODO - Step 2
  def stringDistance(s1: String, s2: String): Int = {
    var d = Array.ofDim[Int](s1.length+1, s2.length+1)
    for(i <- (0 to s1.length)){ d(i)(0) = i}
    for(j <- (0 to s2.length)){ d(0)(j) = j}

    for{i <- 1 to s1.length
        j <- 1 to s2.length}
    {
      d(i)(j) = Math.min(Math.min(d(i-1)(j)+1, d(i)(j-1) + 1), d(i-1)(j-1) + substitutionCost(s1.charAt(i-1), s2.charAt(j-1)))
    }
    d(s1.length)(s2.length)
  }

  /**
   * Helper function to determine cost
   * @param x First character
   * @param y Second character
   * @return 0 if x = y, 1 else
   */
  def substitutionCost(x: Char, y: Char): Int = if(x == y) 0 else 1

  /**
    * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
    * function. If the word is a number, this function just returns it.
    * @param misspelledWord the misspelled word to correct
    * @return the closest word from "misspelledWord"
    */
  // TODO - Step 2
  def getClosestWordInDictionary(misspelledWord: String): String = misspelledWord match {
    case a if a startsWith "_" => misspelledWord
    case b if b forall Character.isDigit => misspelledWord
    case _ => findClosest(misspelledWord)
  }

  def findClosest(m: String): String = {
    val dico = dictionary.map(pair => pair._1 -> stringDistance(pair._1, m)).toList.sortBy(_._2)
    //we discard every pair where the int is not equal to the smallest int
    def filterDico(l: List[(String, Int)]): List[(String, Int)] = l match {
      case Nil => List()
      case x::xs => if(xs.isEmpty) List(x) else if(x._2 != xs.head._2) List(x) else x::filterDico(xs)
    }
    filterDico(dico).sortBy(_._1).head._1
  }
}

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
  def stringDistance(s1: String, s2: String): Int = {

    def LevenshteinDistance(firstWord: String, secondWord: String): Int = {

      // Check if one of the word is empty (base case)
      if (firstWord.isEmpty) return secondWord.length
      if (secondWord.isEmpty) return firstWord.length

      val firstWordReduced = firstWord.substring(0, firstWord.length-1)
      val secondWordReduced = secondWord.substring(0, secondWord.length-1)

      // Return the minimum between the delete char from firstWord, from secondWord and from both
      Math.min(
        Math.min(
          LevenshteinDistance(firstWordReduced, secondWord) + 1,
          LevenshteinDistance(firstWord, secondWordReduced) + 1
        ),

        // Sum the Levenshtein Distance to the cost of the substitution
        LevenshteinDistance(firstWordReduced, secondWordReduced) +
          substitutionCost(firstWord.charAt(firstWord.length-1), secondWord.charAt(secondWord.length-1))
      )
    }

    LevenshteinDistance(s1, s2)
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
  def getClosestWordInDictionary(misspelledWord: String): String = misspelledWord match {
    case a if a startsWith "_" => misspelledWord
    case b if b forall Character.isDigit => misspelledWord
    case _ => findClosest(misspelledWord)
  }

  def findClosest(m: String): String = {
    val dico = dictionary.map(pair => pair._1 -> stringDistance(pair._1, m)).toList.sortBy(_._2)
    // We discard every pair where the int is not equal to the smallest int
    def filterDico(l: List[(String, Int)], acc: List[(String, Int)]): List[(String, Int)] = l match {
      case Nil => acc
      case x::xs => if(xs.isEmpty) List(x) else if(x._2 != xs.head._2) List(x) else filterDico(xs, x::acc)
    }
    filterDico(dico, List()).minBy(_._1)._1
  }
}

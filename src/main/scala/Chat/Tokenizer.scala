package Chat

import Tokens._
import Utils.Dictionary.dictionary
import Utils.{Dictionary, SpellChecker}
import Utils.SpellChecker._


class Tokenizer(input: String) {

  var tokens: List[String] = List()
  /**
    * Separate the user's input into tokens.
    */
  // TODO - Step 3
  def tokenize(): Unit =
    tokens = removeDiacritic(input) //we remove accents
      .replaceAll("[^a-zA-Z0-9_ ]", " ") // we replace every character that are not alphanumeric or _
    .replaceAll(" +", " ") // replacing multiple blank space
      .split(" ") //splitting the string into a list of words separated by blank spaces
      .toList


  /**
    * Get the next token of the user input, or OEL if there is no more token.
  	* @return a tuple that contains the string value of the current token, and the identifier of the token
    */
  // TODO - Step 3
  def nextToken(): (String, Token) = tokens match {
    case Nil => ("EOL", Tokens.EOL)
    case x::xs => {
      tokens = xs
      if(Dictionary.dictionary.contains(x)){
        dicoCaseHelper(dictionary(x))
      }else if(x.startsWith("_")) {
        (x, Tokens.PSEUDO)
      }else if(x forall Character.isDigit){
        (x, Tokens.NUM)
      }else{
        dicoCaseHelper(dictionary(getClosestWordInDictionary(x)))
      }
    }
  }

  def dicoCaseHelper(str: String): (String, Token) = str match {
    case "bonjour" => ("bonjour", Tokens.BONJOUR)
    case "je" => ("je", Tokens.JE)
    case "vouloir" => ("vouloir", Tokens.VOULOIR)
    case "etre" => ("etre", Tokens.ETRE)
    case "croissant" => ("croissant", Tokens.CROISSANT)
    case "biere" => ("biere", Tokens.BIERE)
    case "et" => ("et", Tokens.ET)
    case "ou" => ("ou", Tokens.OU)
    case "svp" => ("svp", Tokens.UNKNOWN)
    case _ => ("unknown", Tokens.UNKNOWN)
  }

  // This section is taken from here: https://stackoverflow.com/questions/3322152/is-there-a-way-to-get-rid-of-accents-and-convert-a-whole-string-to-regular-lette
  // Since Normalizer.normalize(string, Normalizer.Form.NFD) from java.text.Normalizer didn't work, we took what was common before the introduction of normalizer in Java 6
  /**
    * Mirror of the unicode table from 00c0 to 017f without diacritics.
    */
  private val tab00c0 = "AAAAAAACEEEEIIII" + "DNOOOOO\u00d7\u00d8UUUUYI\u00df" + "aaaaaaaceeeeiiii" + "\u00f0nooooo\u00f7\u00f8uuuuy\u00fey" + "AaAaAaCcCcCcCcDd" + "DdEeEeEeEeEeGgGg" + "GgGgHhHhIiIiIiIi" + "IiJjJjKkkLlLlLlL" + "lLlNnNnNnnNnOoOo" + "OoOoRrRrRrSsSsSs" + "SsTtTtTtUuUuUuUu" + "UuUuWwYyYZzZzZzF"

  /**
    * Returns string without diacritics - 7 bit approximation.
    *
    * @param source string to convert
    * @return corresponding string without diacritics
    */
  def removeDiacritic(source: String): String = {
    val vysl = new Array[Char](source.length)
    var one = 0
    for (i <- 0 until source.length) {
      one = source.charAt(i)
      if (one >= '\u00c0' && one <= '\u017f') one = tab00c0.charAt(one.toInt - '\u00c0')
      vysl(i) = one.toChar
    }
    new String(vysl)
  }
}

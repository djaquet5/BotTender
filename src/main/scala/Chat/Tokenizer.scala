package Chat

import java.text.Normalizer

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
  def tokenize(): Unit = tokens = input/*Normalizer.normalize(input, Normalizer.Form.NFD)*/.replaceAll("[^a-zA-Z0-9_ ]", " ")
    .replaceAll(" +", " ").split(" ").toList

  /**
    * Get the next token of the user input, or OEL if there is no more token.
  	* @return a tuple that contains the string value of the current token, and the identifier of the token
    */
  // TODO - Step 3
  def nextToken(): (String, Token) = tokens match {
    case Nil => ("EOL", Tokens.EOL)
    case x::xs => {
      println(tokens)
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
}

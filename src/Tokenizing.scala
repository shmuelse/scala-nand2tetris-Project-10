package token

import java.io.{File, PrintWriter}

import scala.io._
import Utils.Utils
import Parser.{Parsing, XMLParsing}



  class Tokenizing {

    var helper = new Utils
    var indexOfToken = 0
    var indentLevel = 0
    var tokensList: List[String] = null
    var xmlParser: Parsing = new XMLParsing


    def tokenize(fileName: String, path: String, lines: String): Unit = {

      //Set the path of the new file
      val tokenPath = path.concat("\\" + fileName + "T.xml")
      println("the new path is:\n" + tokenPath)


      tokensList = Source.fromFile(fileName).getLines().toList

      indexOfToken = 0;

      while (indexOfToken < tokensList.length) {

        val tokenContent = helper.getTagContent(tokensList(indexOfToken))

        xmlParser.writeFormatted(tokensList(indexOfToken) + ":", indentLevel)
        xmlParser.writeFormatted(helper.getTokenType(tokensList(indexOfToken)), indentLevel)
        xmlParser.writeFormatted(helper.getTagContent(tokensList(indexOfToken)), indentLevel)

        indexOfToken += 1
      }

      /*  var tokenFile = ""

    val help = new helpFunctions
    var line = lines
    var word = ""
    var ch = ""

    while(!lines.isEmpty) {
      word += line.head
      ch = help.getTokenType(word)
      ch match {
        case "digit" =>
          tokenFile += help.digitToken(line)
      }
    }

    val writer = new java.io.FileWriter(tokenPath)

    writer.write("byby")
    writer.close()*/
    }


  }


  class helpFunctions() {


    def isIntegerConstant(x: String) = x forall Character.isDigit

    def isStringConstant(x: String) = x.matches("""^\"[^\\"]*\"$""")

    def isIdentifier(x: String) = x.matches("""^[^\d][\d\w\_]*""")

    def isCommentLine(x: String) = x.matches("""^\/\/.*""")


    /*  def check (word: String): String = {
      if(isIntegerConstant(word))
        return "digit"
      return ""
    }*/


    //Function to check the type of the token
    def getTokenType(tokenType: String): String = {
      val isKeyword = List(
        "Class", "constructor", "function", "method", "field", "static", "var", "int", "char", "boolean", "void", "true", "false"
        , "null", "this", "let", "do", "if", "else", "while", "return")

      val isSymbol = List("{", "}", "(", ")", "[", "]", ".", ",", ";", "+", "-", "*", "/", "&", "", "", "<", ">", "=", "~")

      if (isKeyword.indexOf(tokenType) >= 0)
        return "keyword"
      if (isSymbol.indexOf(tokenType) >= 0)
        return "symbol"
      if (isIntegerConstant(tokenType))
        return "integerConstant"
      if (isStringConstant(tokenType))
        return "stringConstant"
      if (isIdentifier(tokenType))
        return "identifier"
      return ""
    }

    def digitToken(line: String): Unit = {
      var word = ""
      var l = line
      while (isIntegerConstant(l.head.toString)) {
        word += line.head
        l = l.substring(1, line.length)
      }
      return ""
    }

  }


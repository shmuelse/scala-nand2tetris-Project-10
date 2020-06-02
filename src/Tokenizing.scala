package token
import java.io.{File,PrintWriter}
import scala.io._
//import helpToken.HelpTokenizing

class helpFunctions() {


  def isIntegerConstant(x: String) = x forall Character.isDigit

  def isStringConstant(x: String) = x.matches("""^\"[^\\"]*\"$""")

  def isIdentifier(x: String) = x.matches("""^[^\d][\d\w\_]*""")

  def isCommentLine(x:String) = x.matches("""^\/\/.*""")



/*  def check (word: String): String = {
    if(isIntegerConstant(word))
      return "digit"
    return ""
  }*/


  //Function to check the type of the token
  def getTokenType (tokenType: String):String = {
    val isKeyword = List(
      "Class", "constructor","function","method","field","static","var","int","char","boolean","void","true","false"
      ,"null","this","let","do","if","else","while","return")

    val isSymbol = List("{", "}", "(", ")", "[", "]", ".", ",", ";", "+", "-", "*", "/", "&", "", "", "<", ">", "=", "~")

    if(isKeyword.indexOf(tokenType) >= 0)
      return "keyword"
    if(isSymbol.indexOf(tokenType) >= 0)
      return "symbol"
    if(isIntegerConstant(tokenType))
      return "integerConstant"
    if(isStringConstant(tokenType))
      return "stringConstant"
    if(isIdentifier(tokenType))
      return "identifier"
    return ""
  }

  def digitToken (line: String): Unit ={
    var word = ""
    var l = line
    while(isIntegerConstant(l.head.toString)) {
      word += line.head
      l = l.substring(1, line.length)
    }
    return ""
  }

}

class Tokenizing {

  //var tokensList:List[String] = null
  //var tokenIndex = 0;
  //var help = new helpFunctions
  def tokenize(fileName: String, path: String, lines: String): Unit = {
    val tokenFilePath = "\\" + fileName + "T.xml"
    val tokenPath = path.concat(tokenFilePath)

    println("the new path is:\n" + tokenPath)

    //tokensList = Source.fromFile(fileName).getLines().toList

   var tokenFile = ""

    val help = new helpFunctions
    var line = lines
    var word = ""
    var ch = ""

   /* while (tokenIndex < tokensList.length) {
      //val tokenContent =

      }
    }*/
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
    writer.close()
  }


}

class Parsing{


}
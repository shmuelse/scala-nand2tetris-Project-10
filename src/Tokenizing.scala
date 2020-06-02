package token

import helpToken.HelpTokenizing
class helpFunctions() {


  def isIntegerConstant(x: String) = x forall Character.isDigit

  def isStringConstant(x: String) = x.matches("""^\"[^\\"]*\"$""")

  def isIdentifier(x: String) = x.matches("""^[^\d][\d\w\_]*""")

  def isCommentLine(x:String) = x.matches("""^\/\/.*""")


  def check (word: String): String = {
    if(isIntegerConstant(word))
      return "digit"
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
  def tokenize(fileName: String, path: String, lines: String): Unit = {
    val tokenFilePath = "\\" + fileName + "T.xml"
    val tokenPath = path.concat(tokenFilePath)

    println("the new path is:\n" + tokenPath)

    var tokenFile = ""

    val help = new helpFunctions
    var line = lines
    var word = ""
    var ch = ""

    while(!lines.isEmpty) {
      word += line.head
      ch = help.check(word)
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

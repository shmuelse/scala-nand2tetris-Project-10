package token

import helpToken.HelpTokenizing
class helpFunctions() {
  def integerConstant(x: String) = x forall Character.isDigit
}

class Tokenizing {
  def tokenize(fileName: String, path: String, lines: String): Unit = {
    val tokenFilePath = "\\" + fileName + "T.xml"
    val tokenPath = path.concat(tokenFilePath)
    println("the new path is:\n" + tokenPath)
    var line = lines
    var word = ""
    while(!lines.isEmpty) {
      word += line.head
      line = line.substring(1, line.length)

    }

    val writer = new java.io.FileWriter(tokenPath)

    writer.write("byby")
    writer.close()
  }
}

// מגישים: יונתן פרידמן 211820501, איסאיס מולה 321166894, שמואל סגל 052970464
import java.io.{File, PrintWriter}

import scala.io._

object EX_04 {

  var indexOfToken = 0
  var tokensList: List[String] = _

  var xmlWriter: java.io.PrintWriter = _
  var tokenizing = new Tokenizing
  val help = new HelpFunctions


  class HelpFunctions {


  // ***************** Check Functions ******************** //

  def isIntegerConstant(x: String): Boolean = x.matches("^\\d*\\.{0,1}\\d+$")

  def isStringConstant(x: String): Boolean = x.matches("""^\"[^\\"]*\"$""")

  def isIdentifier(x: String): Boolean = x.matches("""^[^\d][\d\w\_]*""")

  def isCommentLine(x: String): Boolean = x.matches("""^\/\/.*""")

  def hasJackFileExtension(x: String): Boolean = x.matches("^.*\\.jack$")


  // ***************** Help Functions ******************** //


    /**
     *
     * @param tokenType
     * @return the type of the token: ( keyword, symbol etc..).
     */
  def getTokenType(tokenType: String): String = {
    val keywordList = List("class", "constructor", "function", "method", "field", "static", "var", "int", "char", "boolean", "void", "true", "false", "null", "this", "let", "do", "if", "else", "while", "return")

    val symbolList = List("{", "}", "(", ")", "[", "]", ".", ",", ";", "+", "-", "*", "/", "&", "", "", "<", ">", "=", "~")

    if (keywordList.indexOf(tokenType) >= 0)
      return "keyword"
    if (symbolList.indexOf(tokenType) >= 0)
      return "symbol"
    if (isIntegerConstant(tokenType))
      return "integerConstant"
    if (isStringConstant(tokenType))
      return "stringConstant"
    if (isIdentifier(tokenType))
      return "identifier"
     ""
  }

  /**
   * This function checks if the parameter is a special
   * character like "&" etc. and returns us what needs
   * to replace the character in the XML file
   *
   * @param str is the param to print between the xml contents
   * @param tokenType
   * @return Alternative value in case needed...
   */
  def getContent(str: String, tokenType: String): String = {

    var stringList = str

    if (tokenType == "stringConstant") {
      stringList = stringList.drop(1)
      stringList = stringList.dropRight(1)
    }
    stringList match {
      case "<" =>  "&lt;"
      case ">" =>  "&gt;"
      case "&" =>  "&amp;"
      case "'" =>  "&apos;"
      case """""" =>  "&quot;"
      case _ =>  stringList
    }

  }

  /**
   *
   * @param token
   * @return
   */
  def getTagContent(token: String): String = {
    val matcher = """\<.*\>\s(.*?)\s\<.*\>""".r
    matcher findFirstIn token match {
      case Some(matcher(inside)) =>  inside
      case _ =>  ""
    }
  }


  }

  class Tokenizing {


  /**
   * @param str is the var to print between thw xml contents
   * @return ready string with the type of the token for XML node
   */
  def writeXmlNode(str: String): String = {
    val tokenType: String = help.getTokenType(str)
     "<" + tokenType + "> " + help.getContent(str, tokenType) + " </" + tokenType + ">"
  }


  /**
   * Creates an XML file in the path the user inserted
   *
   * @param fileName is the path and file name
   */
  def createXMLFile(fileName: String): Unit = {

    var isComment: Boolean = false

    val fileNameStr: String = fileName.replace(".jack", "T.xml")
    val writer = new PrintWriter(new File(fileNameStr))

    val delimiterReg = """(?:\/\/.*|\/\*|\*\/|\<|\>|\.|#|&|\,|:|\*|\(|\)|=|\{|\}|\(|\)|\[|\]|\.|\;|\+|\-|\*|\/|\&|\|\|\=|\~|\"[^\"]*\"|\d+\.{0,1}\d*|\s|\n|\w+)?""".r

    writer.write("<tokens>" + "\n")

    Source.fromFile(fileName).getLines().foreach {
      line => {
        val items = delimiterReg findAllIn line
        items.foreach { x =>
          if(x == "/*") isComment = true
          if(!isComment && x.trim != "" && !help.isCommentLine(x)){
            writer.write(writeXmlNode(x) + "\n")
          }
          if(x == "*/") isComment = false
        }
      }
    }
    writer.write("</tokens>" + "\n")
    writer.close()
  }

  }


  def main(args: Array[String]) {

    println("Enter file path:")

    val path :String = new java.io.File(scala.io.StdIn.readLine()).getCanonicalPath + "\\"
    println("path is:\n" + path)

    refArrayOps(new File(path).listFiles).foreach {
      file =>
        if (help.hasJackFileExtension(file.getName)) {

          tokenizing.createXMLFile(path + file.getName)
        }
        else {
          println("Not A JACK File\n")
        }

    }
  }

}

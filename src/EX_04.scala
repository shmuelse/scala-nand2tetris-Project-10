import java.io.{File, PrintWriter}

import scala.io.Source

object EX_04 {

  var indexOfToken = 0
  var tokensList: List[String] = null

  var indentLevel = 0
  var xmlWriter: java.io.PrintWriter = null
  var xmlParser = new XMLParsing
  var tokenizing = new Tokenizing
  val help = new HelpFunctions



  class HelpFunctions {


    // ***************** Check Functions ******************** //

    def isIntegerConstant(x: String) = x forall Character.isDigit

    def isStringConstant(x: String) = x.matches("""^\"[^\\"]*\"$""")

    def isIdentifier(x: String) = x.matches("""^[^\d][\d\w\_]*""")

    def isCommentLine(x: String) = x.matches("""^\/\/.*""")

    def hasJackFileExtention(x: String) = x.matches("^.*\\.jack$")


    // ***************** Help Functions ******************** //

    def getTokenType(tokenType: String): String = {
      val keywordList = List(
        "Class", "constructor", "function", "method", "field", "static", "var", "int", "char", "boolean", "void",
        "true", "false", "null", "this", "let", "do", "if", "else", "while", "return")

      val symbolList = List("{", "}", "(", ")", "[", "]", ".", ",", ";", "+", "-", "*", "/", "&", "", "", "<",
        ">", "=", "~")

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
      return ""
    }

    def getContent(str: String, tokenType: String): String = {

      var stringList = str;

      if (tokenType == "stringConstant") {
        stringList = stringList.drop(1)
        stringList = stringList.dropRight(1)
      }
      stringList match {
        case "<" => return "&lt;"
        case ">" => return "&gt;"
        case "&" => return "&amp;"
        case "'" => return "&apos;"
        case """""" => return "&quot;"
        case _ => return stringList
      }

    }

    def getTagContent(token: String): String = {
      val matcher = """\<.*\>\s(.*?)\s\<.*\>""".r
      matcher findFirstIn token match {
        case Some(matcher(inside)) => return inside
        case _ => return ""
      }
    }

    def writeFormatted(str: String): Unit = {
      xmlWriter.write("  " * indentLevel + str + "\n")

    }

  }

  class XMLParsing {


    //return the type of the token ready for XML file
    def writeXmlNode(str: String): String = {
      val tokenName: String = help.getTokenType(str)
      return ("<" + tokenName + "> " + help.getContent(str, tokenName) + " </" + tokenName + ">")
    }

    // Create the Xml file
    def createXMLFile(fileName: String) = {

      var isComment: Boolean = false;

      val writer = new PrintWriter(new File(fileName.replace(".jack", "T.xml")))

      val delimiterReg = """(?:\/\/.*|\/\*|\*\/|\<|\>|\.|#|&|\,|:|\*|\(|\)|=|\{|\}|\(|\)|\[|\]|\.|\;|\+|\-|\*|\/|\&|\|\|\=|\~|\"[^\"]*\"|\d+\.{0,1}\d*|\s|\n|\w+)?""".r

      writer.write("<tokens>" + "\n")

      Source.fromFile(fileName).getLines().foreach {
        line => {
          val items = delimiterReg findAllIn line
          items.foreach { x =>
            if (x == "/*") isComment = true;
            if (!isComment && x.trim != "" && !help.isCommentLine(x)) {
              writer.write(writeXmlNode(x) + "\n")
            }
            if (x == "*/") isComment = false
          }
        }
      }
      writer.write("</tokens" + "\n")
      writer.close()
    }






  }

  class Tokenizing {

    def tokenize(fileName: String, path: String, lines: String): Unit = {

      val tokenPath = path.concat("\\" + fileName + "T.xml")
      println("the new path is:\n" + tokenPath)

      tokensList = Source.fromFile(tokenPath).getLines().toList
      indexOfToken = 0


      while (indexOfToken < tokensList.length) {

        help.writeFormatted(tokensList(indexOfToken) + ":")
        help.writeFormatted(help.getTokenType(tokensList(indexOfToken)))
        help.writeFormatted(help.getTagContent(tokensList(indexOfToken)))

        indexOfToken += 1
      }
    }
  }

  object main extends App {
    println("Enter file path:")

    val path = new java.io.File(scala.io.StdIn.readLine()).getCanonicalPath
    println("path is:\n" + path)


    var str = ""
    refArrayOps(new File(path).listFiles).foreach {
      file => {
        if (!help.hasJackFileExtention(file.getPath)) {
          println("Not A JACK File\n")
        }
        else {
          val jackFileName = file.getName
          val fileName = jackFileName.replaceAll(".jack", ".xml")
          xmlParser.createXMLFile(path + fileName)
          xmlWriter = new PrintWriter(new File(path + fileName))
          tokenizing.tokenize(file.getName, path, Source.fromFile(file.getPath).mkString)
          xmlWriter.close()

        }
      }
    }
  }

}

import java.io.{File, PrintWriter}

import scala.io.Source

object EX_04 {

  var indexOfToken = 0
  var tokensList: List[String] = null

  var indentLevel = 0
  var xmlWriter: java.io.PrintWriter = null
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

    // Print to the xml file
    def writeFormatted(str: String, indentLevel: Int): Unit = {
      xmlWriter.write("  " * indentLevel + str + "\n")
    }



  }

  class Tokenizing {


    def tokenize(fileName: String, path: String, lines: String): Unit = {

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

  object main extends App {
    println("Enter file path:")

    val read = scala.io.StdIn.readLine()
    val path = new java.io.File(read).getCanonicalPath
    println("path is:\n" + path)


    val tok = new Tokenizing()
    var func = new HelpFunctions
    var xmlParser = new XMLParsing
    var str = ""
    refArrayOps(new File(path).listFiles).foreach {
      file => {

        if (!func.hasJackFileExtention(file.getPath)) {

          println("Not A JACK File\n")
        } else {
          val jackFileName = file.getName
          val fileName = jackFileName.replaceAll(".jack", "")
          xmlParser.createXMLFile(path + fileName)
          xmlParser
          val lines = Source.fromFile(file.getPath).mkString //.replaceAll(" ", "")

          println("the file is:\n" + fileName)
          tok.tokenize(fileName, path, lines)
        }
      }
    }
  }

}

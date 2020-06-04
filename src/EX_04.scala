import java.io.{File, PrintWriter}

import scala.io.Source

object EX_04 {

  var indexOfToken = 0
  var tokensList: List[String] = null

  var indentLevel = 0
  var xmlWriter: java.io.PrintWriter = null
  var tokenizing = new Tokenizing
  var parsing = new Parsing
  val help = new HelpFunctions


  class HelpFunctions {


    // ***************** Check Functions ******************** //

    def isIntegerConstant(x: String) = x forall Character.isDigit

    def isStringConstant(x: String) = x.matches("""^\"[^\\"]*\"$""")

    def isIdentifier(x: String) = x.matches("""^[^\d][\d\w\_]*""")

    def isCommentLine(x: String) = x.matches("""^\/\/.*""")

    def hasJackFileExtention(x: String) = x.matches("^.*\\.jack$")


    // ***************** Help Functions ******************** //

    /**
     *
     * @param tokenType
     * @return the type of the token
     */
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

    /**
     *
     * @param token
     * @return
     */
    def getTagContent(token: String): String = {
      val matcher = """\<.*\>\s(.*?)\s\<.*\>""".r
      matcher findFirstIn token match {
        case Some(matcher(inside)) => return inside
        case _ => return ""
      }
    }


    /**
     * Indents the contents of the XML file
     *
     * @param str
     */
    def writeFormatted(str: String): Unit = {
      xmlWriter.write("  " * indentLevel + str + "\n")

    }

  }

  class Tokenizing {

    /**
     * @param str is the var to print between thw xml contents
     * @return ready string with the type of the token for XML node
     */
    def writeXmlNode(str: String): String = {
      val tokenType: String = help.getTokenType(str)
      return ("<" + tokenType + "> " + help.getContent(str, tokenType) + " </" + tokenType + ">")
    }

    /**
     * Creates an XML file in the path the user inserted
     *
     * @param fileName is the path and file name
     */
    def createXMLFile(fileName: String) = {

      val writer = new PrintWriter(new File(fileName.replace(".jack", "T.xml")))

      val delimiterReg = """(?:\/\/.*|\/\*|\*\/|\<|\>|\.|#|&|\,|:|\*|\(|\)|=|\{|\}|\(|\)|\[|\]|\.|\;|\+|\-|\*|\/|\&|\|\|\=|\~|\"[^\"]*\"|\d+\.{0,1}\d*|\s|\n|\w+)?""".r

      var isComment: Boolean = false;

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

  class Parsing {

    val subOpenings = List("constructor", "function", "method")
    val statStarts = List("do", "while", "let", "if", "return")
    val opList = List("+", "-", "*", "/", "&amp;", "|", "&lt;", "&gt;", "=")

    /**
     *
     * @param fileName
     */
    def parser(fileName: String): Unit = {

      val tokenPath: String = fileName.replace(".jack", "T.xml")
      println("the new path is:\n" + tokenPath)

      tokensList = Source.fromFile(tokenPath).getLines().toList
      indexOfToken = 0

      while (indexOfToken < tokensList.length) {

        val tokenContent = help.getTagContent(tokensList(indexOfToken))
        if (tokenContent == "class")
          classParser()
        indexOfToken += 1
      }
    }

    /**
     *
     */
    def classParser(): Unit = {
      help.writeFormatted("<class>")
      indentLevel += 1
      help.writeFormatted(tokensList(indexOfToken)) //<keyword> class </keyword>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<identifier> Main </identifier>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<symbol> { </symbol>
      indexOfToken += 1
      varDeclaration()
      while (tokensList(indexOfToken) != null && help.getTokenType(tokensList(indexOfToken)) == "keyword" && subOpenings.indexOf(help.getTagContent(tokensList(indexOfToken))) >= 0)
        subroutine()

      help.writeFormatted(tokensList(indexOfToken)) //<symbol> } </symbol>
      indexOfToken += 1
      indentLevel -= 1
      help.writeFormatted("</class>")
      indexOfToken += 1

    }

    /**
     *
     */
    def varDeclaration(): Unit = {
      while (help.getTagContent(tokensList(indexOfToken)) == "static"
        || help.getTagContent(tokensList(indexOfToken)) == "field") {
        help.writeFormatted("<classVarDec>")
        indentLevel += 1
        help.writeFormatted(tokensList(indexOfToken)) //<keyword> field or static </keyword>
        indexOfToken += 1
        help.writeFormatted(tokensList(indexOfToken)) //<keyword> int </keyword>
        indexOfToken += 1
        help.writeFormatted(tokensList(indexOfToken)) //<identifier> x </identifier>
        indexOfToken += 1

        while (help.getTagContent(tokensList(indexOfToken)) == ",") {
          help.writeFormatted(tokensList(indexOfToken)) // <symbol> , </symbol>
          indexOfToken += 1
          help.writeFormatted(tokensList(indexOfToken)) // <identifier> y </identifier>
          indexOfToken += 1
        }

        help.writeFormatted(tokensList(indexOfToken)) // <symbol> ; </symbol>
        indexOfToken += 1
        indentLevel -= 1
        help.writeFormatted("</classVarDec>")
      }
    }

    /**
     *
     */
    def subroutine(): Unit = {

      help.writeFormatted("<subroutineDec>")
      indentLevel += 1
      help.writeFormatted(tokensList(indexOfToken)) //<keyword> 'constructor', 'function', or 'method' </keyword>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<keyword>void</keyword>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<identifier>main</identifier>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<symbol>(</symbol>
      indexOfToken += 1
      help.writeFormatted("<parameterList>")

      if (help.getTagContent(tokensList(indexOfToken)) != ")")
        subParameters()

      help.writeFormatted("</parameterList>")

      help.writeFormatted(tokensList(indexOfToken)) //<symbol>)</symbol>
      indexOfToken += 1

      help.writeFormatted("<subroutineBody>")
      indentLevel += 1
      help.writeFormatted(tokensList(indexOfToken)) //<symbol>{</symbol>
      indexOfToken += 1

      varDeclaration()
      statements()

      help.writeFormatted(tokensList(indexOfToken)) //<symbol>}</symbol>
      indexOfToken += 1
      indentLevel -= 1
      help.writeFormatted("</subroutineBody>")
      indentLevel -= 1
      help.writeFormatted("</subroutineDec>")

    }

    /**
     *
     */
    def subParameters(): Unit = {
      indentLevel += 1
      help.writeFormatted(tokensList(indexOfToken)) //<keyword> int </keyword>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<identifier> x </identifier>
      indexOfToken += 1
      while (help.getTagContent(tokensList(indexOfToken)) == ",") {
        subroutineParameter()
      }
      indentLevel -= 1
    }

    /**
     *
     */
    def subroutineParameter(): Unit = {
      help.writeFormatted(tokensList(indexOfToken)) //<keyword> int </keyword>
      indexOfToken +=1
      help.writeFormatted(tokensList(indexOfToken)) //<identifier> x </identifier>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<symbol>}</symbol>
      indexOfToken += 1

    }

    /**
     *
     */
    def statements(): Unit = {
      help.writeFormatted("<statements>")
      indentLevel += 1

      while (statStarts.indexOf(help.getTagContent(tokensList(indexOfToken))) >= 0)
        statement()

      indentLevel -= 1
      help.writeFormatted("</statements>")
    }

    /**
     *
     */
    def statement(): Unit = {

      help.getTagContent(tokensList(indexOfToken)) match {
        case "do" =>
          doStatement();
        case "while" =>
          whileStatement();
        case "if" =>
          ifStatement();
        case "return" =>
          returnStatement();
        case "let" =>
          letStatement();
      }
    }

    /**
     *
     */
    def letStatement(): Unit = {
      help.writeFormatted("<letStatement>")
      indentLevel += 1
      help.writeFormatted(tokensList(indexOfToken)) //<keyword> let </keyword>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<keyword> game </keyword>
      indexOfToken += 1
      if (help.getTagContent(tokensList(indexOfToken)) == "[") {
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> [ </symbol>
        indexOfToken += 1
        expression()
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> ] </symbol>
        indexOfToken += 1
      }
      help.writeFormatted(tokensList(indexOfToken)) //<symbol> = </symbol>
      indexOfToken += 1
      expression()
      help.writeFormatted(tokensList(indexOfToken)) //<symbol> ; </symbol>
      indexOfToken += 1
      indentLevel -= 1
      help.writeFormatted("</letStatement>")

    }

    /**
     *
     */
    def returnStatement(): Unit = {
      help.writeFormatted("<returnStatement>")
      indentLevel += 1
      help.writeFormatted(tokensList(indexOfToken)) //<keyword> return </keyword>
      indexOfToken += 1
      if (help.getTagContent(tokensList(indexOfToken)) != ";")
        expression()
      help.writeFormatted(tokensList(indexOfToken)) //<symbol> ; </symbol>
      indexOfToken += 1
      indentLevel -= 1
      help.writeFormatted("</returnStatement>")
    }

    /**
     *
     */
    def ifStatement(): Unit = {

      help.writeFormatted("<ifStatement>")
      indentLevel += 1
      help.writeFormatted(tokensList(indexOfToken)) //<keyword> if </keyword>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<symbol> ( </symbol>
      indexOfToken += 1
      expression;
      help.writeFormatted(tokensList(indexOfToken)) //<symbol> ) </symbol>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<symbol> { </symbol>
      indexOfToken += 1
      statements;
      help.writeFormatted(tokensList(indexOfToken)) //<symbol> } </symbol>
      indexOfToken += 1
      if (help.getTagContent(tokensList(indexOfToken)) == "else") {
        help.writeFormatted(tokensList(indexOfToken)) //<keyword> if </keyword>
        indexOfToken += 1
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> { </symbol>
        indexOfToken += 1
        statements;
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> } </symbol>
        indexOfToken += 1
      }
      indentLevel -= 1
      help.writeFormatted("</ifStatement>")
    }

    /**
     *
     */
    def whileStatement(): Unit = {
      help.writeFormatted("<whileStatement>")
      indentLevel += 1
      help.writeFormatted(tokensList(indexOfToken)) //<keyword> while </keyword>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<symbol> ( </symbol>
      indexOfToken += 1
      expression()
      help.writeFormatted(tokensList(indexOfToken)) //<symbol> ) </symbol>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<symbol> { </symbol>
      indexOfToken += 1
      statements()
      help.writeFormatted(tokensList(indexOfToken)) //<symbol> } </symbol>
      indexOfToken += 1
      indentLevel -= 1
      help.writeFormatted("</whileStatement>")
    }

    /**
     *
     */
    def doStatement(): Unit = {
      help.writeFormatted("<doStatement>")
      indentLevel += 1
      help.writeFormatted(tokensList(indexOfToken)) //<keyword> do </keyword>
      indexOfToken += 1
      subroutineCall()
      help.writeFormatted(tokensList(indexOfToken)) //<symbol> ; </symbol>
      indexOfToken += 1
      indentLevel -= 1
      help.writeFormatted("</doStatement>")
    }

    /**
     *
     */
    def expression(): Unit = {
      help.writeFormatted("<expression>")
      indentLevel += 1
      term;
      while (opList.indexOf(help.getTagContent(tokensList(indexOfToken))) >= 0) {
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> + </symbol>
        indexOfToken += 1
        term;
      }
      indentLevel -= 1
      help.writeFormatted("</expression>")
    }

    /**
     *
     */
    def subroutineCall(): Unit = {
      help.writeFormatted(tokensList(indexOfToken)) //<identifier>SquareGame</identifier>
      indexOfToken += 1
      if (help.getTagContent(tokensList(indexOfToken)) == "(") {
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> ( </symbol>
        indexOfToken += 1
        expressionList()
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> ) </symbol>
        indexOfToken += 1
      }
      else {
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> . </symbol>
        indexOfToken += 1
        help.writeFormatted(tokensList(indexOfToken)) //<identifier>SquareGame</identifier>
        indexOfToken += 1
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> ( </symbol>
        indexOfToken += 1
        expressionList()
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> ) </symbol>
        indexOfToken += 1
      }
    }

    /**
     * recursive
     */
    def term(): Unit = {

      help.writeFormatted("<term>")
      indentLevel += 1
      if (help.getTagContent(tokensList(indexOfToken)) == "(") {
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> ( </symbol>
        indexOfToken += 1
        expression()
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> ) </symbol>
        indexOfToken += 1
      }
      else if (help.getTagContent(tokensList(indexOfToken + 1)) == "[") {
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> varName </symbol>
        indexOfToken += 1
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> [ </symbol>
        indexOfToken += 1
        expression()
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> ] </symbol>
        indexOfToken += 1
      }
      else if ((help.getTagContent(tokensList(indexOfToken)) == "-") || (help.getTagContent(tokensList(indexOfToken)) == "~")) {
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> unary op </symbol>
        indexOfToken += 1
        term()
      }
      else if ((help.getTagContent(tokensList(indexOfToken + 1)) == "(") || (help.getTagContent(tokensList(indexOfToken + 1)) == ".")) {
        subroutineCall()
      }

      else {
        help.writeFormatted(tokensList(indexOfToken)) //<indentifier>  </indentifier>
        indexOfToken += 1
      }
      indentLevel -= 1
      help.writeFormatted("</term>")

    }

    /**
     *
     */
    def expressionList(): Unit = {
      help.writeFormatted("<expressionList>")
      indentLevel += 1
      if (help.getTagContent(tokensList(indexOfToken)) != ")") {
        expression()
        while (help.getTagContent(tokensList(indexOfToken)) == ",") {
          help.writeFormatted(tokensList(indexOfToken)) //<symbol> , </symbol>
          indexOfToken += 1
          expression()
        }
      }
      indentLevel -= 1
      help.writeFormatted("</expressionList>")
    }
  }


  object main extends App {

    println("Enter file path:")

    val path = new java.io.File(scala.io.StdIn.readLine()).getCanonicalPath
    println("path is:\n" + path)


    refArrayOps(new File(path).listFiles).foreach {
      file => {
        if (!help.hasJackFileExtention(file.getPath)) {
          println("Not A JACK File\n")
        }
        else {
          val jackFileName = file.getName
          val fileName = jackFileName.replaceAll(".jack", ".xml")
          tokenizing.createXMLFile(path + file.getName)
          xmlWriter = new PrintWriter(new File(path + fileName))
          parsing.parser(path + file.getName)
          xmlWriter.close()

        }
      }
    }
  }

}

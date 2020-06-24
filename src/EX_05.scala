// מגישים: יונתן פרידמן 211820501, איסאיס מולה 321166894, שמואל סגל 052970464
import java.io.{File, PrintWriter}

import scala.collection.mutable.ListBuffer
import scala.io.Source

object EX_05 {

  var indexOfToken = 0
  var tokensList: List[String] = _
  var indexOfXml = 0
  var xmlList: List[String] = _

  var indentLevel = 0
  var xmlWriter: java.io.PrintWriter = _
  var vmWriter: java.io.PrintWriter = _
  var tokenizing = new Tokenizing
  var parsing = new Parsing
  var translating = new JACKtoVM
  val help = new HelpFunctions

  var classTable: SymbolTable = _
  var methodTable: SymbolTable = _

  var className = ""
  var subType = ""
  var subName = ""

  // ***************** Ex_5 ******************** //

  class SymbolEntry /*(aName: String, symType: String, symSegment: String, num: Int)*/ {
    var name = ""
    var symbolType = ""
    var symbolSegment = ""
    var offset = 0

    def construct (aName: String, symType: String, segment: String, num: Int) {
      name = aName
      symbolType = symType
      symbolSegment = segment
      offset = num
    }

    def getName: String = {
      return name
    }

    def getType: String = {
      return symbolType
    }

    def getSegment: String = {
      return symbolSegment
    }

    def getOffset: Int = {
      return offset
    }
  }

  class SymbolTable {
    var table: ListBuffer[SymbolEntry] = _
    var symbol: SymbolEntry = _

    def addRow(name: String, symType: String, segment: String): Unit = {
      if(table.contains(symbol.getSegment == segment)){
        val index = table.lastIndexWhere((entry) => entry.getSegment == segment)
        val offset = table.apply(index).getOffset + 1
        symbol.construct(name, symType, segment, offset)
        table.addOne(symbol)
      } else {
        symbol.construct(name, symType, segment, 0)
      }
    }

    def clearTable(): Unit = {
      table.clear()
    }

    def typeOf(name: String): Unit = {
      val index = table.indexWhere(entry => entry.getName == name)
      return table.apply(index).getType
    }

    def segmentOf(name: String): Unit = {
      val index = table.indexWhere(entry => entry.getName == name)
      return table.apply(index).getSegment
    }

    def indexOf(name: String): Unit = {
      val index = table.indexWhere(entry => entry.getName == name)
      return table.apply(index).getOffset
    }

    def varCount(segment: String): Unit = {
      if(table.contains(symbol.getSegment == segment)){
        val index = table.lastIndexWhere((entry) => entry.getSegment == segment)
        val offset = table.apply(index).getOffset + 1
        return offset
      } else {
        return 0
      }
    }
  }

/*
  class JACKtoVM {
    var someName  = ""
    var someType = ""
    var someSegment = ""

    def translate(fileName: String): Unit = {
      val xmlPath: String = fileName.replace(".jack", ".xml")
      println("the new path is:\n" + xmlPath)

      xmlList = Source.fromFile(xmlPath).getLines().toList
      indexOfXml = 0

      while (indexOfXml < xmlList.length) {

        val parseContent = xmlList(indexOfXml)
        if (parseContent == "<class>")
          classTranslate()
        indexOfXml += 1
      }
    }

    def classTranslate(): Unit = {
      classTable.clearTable()
      indexOfXml += 1
      indexOfXml += 1
      indexOfXml += 1
      classVarDecT()
    }

    def classVarDecT(): Unit = {
      while (xmlList(indexOfXml) == "<classVarDec>") {
        //<classVarDec>
        indexOfXml += 1
        //<keyword> field or static </keyword>
        someSegment = help.getTagContent(xmlList(indexOfXml))
        indexOfXml += 1
        //<keyword> int </keyword>
        someType = help.getTagContent(xmlList(indexOfXml))
        indexOfXml += 1
        //<identifier> x </identifier>
        someName = help.getTagContent(xmlList(indexOfXml))
        indexOfXml += 1
        classTable.addRow(someName, someType, someSegment)

        while (help.getTagContent(xmlList(indexOfXml)) == ",") {
           // <symbol> , </symbol>
          indexOfXml += 1
           // <identifier> y </identifier>
          someName = help.getTagContent(xmlList(indexOfXml))
          indexOfXml += 1
          classTable.addRow(someName, someType, someSegment)
        }

         // <symbol> ; </symbol>
        indexOfXml += 1
        //</classVarDec>
        indexOfXml += 1
      }


    }


  }
*/

  // ***************** Ex_4 ******************** //

  class HelpFunctions {


    // ***************** Check Functions ******************** //

    def isIntegerConstant(x: String): Boolean = x forall Character.isDigit

    def isStringConstant(x: String): Boolean = x.matches("""^\"[^\\"]*\"$""")

    def isIdentifier(x: String): Boolean = x.matches("""^[^\d][\d\w\_]*""")

    def isCommentLine(x: String): Boolean = x.matches("""^\/\/.*""")

    def hasJackFileExtention(x: String): Boolean = x.matches("^.*\\.jack$")


    // ***************** Help Functions - Tokenizing ******************** //

    /**
     *
     * @param tokenType
     * @return the type of the token: ( keyword, symbol etc..).
     */
    def getTokenType(tokenType: String): String = {
      val keywordList = List(
        "class", "constructor", "function", "method", "field", "static", "var", "int", "char", "boolean", "void",
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
        case Some(matcher(inside)) => inside
        case _ =>  ""
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

    // ***************** Help Functions - Parsing ******************** //

    def writeSubOpening(): Unit = {
      xmlWriter.write(s"function ${className}.${subName}\n")
      subType match {
        case "constructor" =>

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

      val fileNameStr:String = fileName.replace(".jack","T.xml")
      val writer = new PrintWriter(new File(fileNameStr))

      val delimiterReg = """(?:\/\/.*|\/\*|\*\/|\<|\>|\.|#|&|\,|:|\*|\(|\)|=|\{|\}|\(|\)|\[|\]|\.|\;|\+|\-|\*|\/|\&|\|\|\=|\~|\"[^\"]*\"|\d+\.{0,1}\d*|\s|\n|\w+)?""".r

      var isComment: Boolean = false

      writer.write("<tokens>" + "\n")

      Source.fromFile(fileName).getLines().foreach {
        line => {
          val items = delimiterReg findAllIn line
          items.foreach { x =>
            if(x == "/*") isComment = true

            if(!isComment && x.trim != "" && !help.isCommentLine(x))
              writer.write(writeXmlNode(x) + "\n")

            if(x == "*/") isComment = false
          }
        }
      }
      writer.write("</tokens>" + "\n")
      writer.close()
    }

  }

  class Parsing {
    val subOpenings = List("constructor", "function", "method")
    val statStarts = List("do", "while", "let", "if", "return")
    val opList = List("+", "-", "*", "/", "&amp;", "|", "&lt;", "&gt;", "=")

    var someName  = ""
    var someType = ""
    var someSegment = ""

    /**
     *
     * @param fileName is the file directory path
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
      classTable.clearTable()

      /*help.writeFormatted("<class>")
      indentLevel += 1
      help.writeFormatted(tokensList(indexOfToken)) //<keyword> class </keyword>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<identifier> Main </identifier>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<symbol> { </symbol>
      indexOfToken += 1*/

      indexOfToken += 1
      className = help.getTagContent(tokensList(indexOfToken))
      indexOfToken += 1
      indexOfToken += 1


      classVarDeclaration()
      while (/*tokensList(indexOfToken) != null  &&*/ subOpenings.indexOf(help.getTagContent(tokensList(indexOfToken))) >= 0)
        subroutine()

      /*help.writeFormatted(tokensList(indexOfToken)) //<symbol> } </symbol>
      indexOfToken += 1
      indentLevel -= 1
      help.writeFormatted("</class>")
      indexOfToken += 1*/

    }

    /**
     *
     */
    def classVarDeclaration(): Unit = {
      while (help.getTagContent(tokensList(indexOfToken)) == "static"
        || help.getTagContent(tokensList(indexOfToken)) == "field") {
        /*help.writeFormatted("<classVarDec>")
        indentLevel += 1
        help.writeFormatted(tokensList(indexOfToken)) //<keyword> field or static </keyword>
        indexOfToken += 1
        help.writeFormatted(tokensList(indexOfToken)) //<keyword> int </keyword>
        indexOfToken += 1
        help.writeFormatted(tokensList(indexOfToken)) //<identifier> x </identifier>
        indexOfToken += 1*/

        //<keyword> field or static </keyword>
        someSegment = help.getTagContent(tokensList(indexOfToken))
        indexOfToken += 1
        //<keyword> int </keyword>
        someType = help.getTagContent(tokensList(indexOfToken))
        indexOfToken += 1
        //<identifier> x </identifier>
        someName = help.getTagContent(tokensList(indexOfToken))
        indexOfToken += 1
        classTable.addRow(someName, someType, someSegment)


        while (help.getTagContent(tokensList(indexOfToken)) == ",") {
          /*help.writeFormatted(tokensList(indexOfToken)) // <symbol> , </symbol>
          indexOfToken += 1
          help.writeFormatted(tokensList(indexOfToken)) // <identifier> y </identifier>
          indexOfToken += 1*/

          // <symbol> , </symbol>
          indexOfToken += 1
          // <identifier> y </identifier>
          someName = help.getTagContent(tokensList(indexOfToken))
          indexOfToken += 1
          classTable.addRow(someName, someType, someSegment)
        }
        //help.writeFormatted(tokensList(indexOfToken)) // <symbol> ; </symbol>
        indexOfToken += 1
        //indentLevel -= 1
        //help.writeFormatted("</classVarDec>")
      }

    }

    /**
     *
     */
    def subroutine(): Unit = {
      methodTable.clearTable()

      //help.writeFormatted("<subroutineDec>")
      //indentLevel += 1
      //help.writeFormatted(tokensList(indexOfToken)) //<keyword> 'constructor', 'function', or 'method' </keyword>
      /*indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<keyword>void</keyword>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<identifier>main</identifier>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<symbol>(</symbol>
      indexOfToken += 1
      help.writeFormatted("<parameterList>")*/

      subType = help.getTagContent(tokensList(indexOfToken))
      indexOfToken += 1
      indexOfToken += 1
      indexOfToken += 1
      indexOfToken += 1

      // if the subroutine is a method - send a copy of the object to the method table
      if(subType == "method")
        methodTable.addRow("this", className, "argument")

      if (help.getTagContent(tokensList(indexOfToken)) != ")")
        subParameters()

      /*help.writeFormatted("</parameterList>")

      help.writeFormatted(tokensList(indexOfToken)) //<symbol>)</symbol>*/
      indexOfToken += 1

      //help.writeFormatted("<subroutineBody>")
      //indentLevel += 1
      //help.writeFormatted(tokensList(indexOfToken)) //<symbol>{</symbol>
      indexOfToken += 1

      varDeclaration()



      statements()

      //help.writeFormatted(tokensList(indexOfToken)) //<symbol>}</symbol>
      indexOfToken += 1
      /*indentLevel -= 1
      help.writeFormatted("</subroutineBody>")
      indentLevel -= 1
      help.writeFormatted("</subroutineDec>")*/

    }

    /**
     *
     */
    def subParameters(): Unit = {
      //indentLevel += 1
      /*help.writeFormatted(tokensList(indexOfToken)) //<keyword> int </keyword>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<identifier> x </identifier>
      indexOfToken += 1*/

      someSegment = "argument"
      someType = help.getTagContent(tokensList(indexOfToken))
      indexOfToken += 1
      someName = help.getTagContent(tokensList(indexOfToken))
      indexOfToken += 1

      methodTable.addRow(someName, someType, someSegment)

      while (help.getTagContent(tokensList(indexOfToken)) == ",") {
        subroutineParameter()
      }
      //indentLevel -= 1
    }

    /**
     *
     */
    def subroutineParameter(): Unit = {
      /*help.writeFormatted(tokensList(indexOfToken)) //<symbol> , </symbol>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<keyword> int </keyword>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<identifier> x </identifier>
      indexOfToken += 1*/

      indexOfToken += 1
      someType = help.getTagContent(tokensList(indexOfToken))
      indexOfToken += 1
      someName = help.getTagContent(tokensList(indexOfToken))
      indexOfToken += 1

      methodTable.addRow(someName, someType, someSegment)

    }

    /**
     *
     */
    def varDeclaration(): Unit = {
      while (help.getTagContent(tokensList(indexOfToken)) == "var") {
        /*help.writeFormatted("<varDec>")
        indentLevel += 1
        help.writeFormatted(tokensList(indexOfToken)) //<keyword> var </keyword>
        indexOfToken += 1
        help.writeFormatted(tokensList(indexOfToken)) //<keyword> int </keyword>
        indexOfToken += 1
        help.writeFormatted(tokensList(indexOfToken)) //<identifier> x </identifier>
        indexOfToken += 1*/

        someSegment = "local"
        indexOfToken += 1
        someType = help.getTagContent(tokensList(indexOfToken))
        indexOfToken += 1
        someName = help.getTagContent(tokensList(indexOfToken))
        indexOfToken += 1

        methodTable.addRow(someName, someType, someSegment)

        while (help.getTagContent(tokensList(indexOfToken)) == ",") {
          /*help.writeFormatted(tokensList(indexOfToken)) // <symbol> , </symbol>
          indexOfToken += 1
          help.writeFormatted(tokensList(indexOfToken)) // <identifier> y </identifier>
          indexOfToken += 1*/

          indexOfToken += 1
          someName = help.getTagContent(tokensList(indexOfToken))
          indexOfToken += 1

          methodTable.addRow(someName, someType, someSegment)
        }
        //help.writeFormatted(tokensList(indexOfToken)) // <symbol> ; </symbol>
        indexOfToken += 1
       /* indentLevel -= 1
        help.writeFormatted("</varDec>")*/
      }
    }

    /**
     *
     */
    def statements(): Unit = {
      /*help.writeFormatted("<statements>")
      indentLevel += 1*/

      while (statStarts.indexOf(help.getTagContent(tokensList(indexOfToken))) >= 0)
        statement()

      /*indentLevel -= 1
      help.writeFormatted("</statements>")*/
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
      expression()
      help.writeFormatted(tokensList(indexOfToken)) //<symbol> ) </symbol>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<symbol> { </symbol>
      indexOfToken += 1
      statements()
      help.writeFormatted(tokensList(indexOfToken)) //<symbol> } </symbol>
      indexOfToken += 1
      if (help.getTagContent(tokensList(indexOfToken)) == "else") {
        help.writeFormatted(tokensList(indexOfToken)) //<keyword> else </keyword>
        indexOfToken += 1
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> { </symbol>
        indexOfToken += 1
        statements()
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
      term()
      while (opList.indexOf(help.getTagContent(tokensList(indexOfToken))) >= 0) {
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> + </symbol>
        indexOfToken += 1
        term()
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


  def check(): Unit = {

  }

  def main(args: Array[String]) {


    //Getting the file path from the user
    println("Enter file path:")
    val path = new java.io.File(scala.io.StdIn.readLine()).getCanonicalPath + "\\"
    println("path is:\n" + path)


    refArrayOps(new File(path).listFiles).foreach {
      file => {
        if (help.hasJackFileExtention(file.getName)) {

          // tokenizer creates T.xml file
          tokenizing.createXMLFile(path + file.getName)

          // parser creates .xml file
          var strFileName :String = file.getName.replace(".jack",".xml")
          xmlWriter = new PrintWriter(new File(path + strFileName))
          parsing.parser(path + file.getName)
          xmlWriter.close()

          // translator creates .vm file
          strFileName = file.getName.replace(".jack",".vm")
          vmWriter = new PrintWriter(new File(path + strFileName))
          translating.translate(path + file.getName)
          vmWriter.close()
        }
        else {
          println("Not A JACK File\n")
        }
      }
    }
  }
}

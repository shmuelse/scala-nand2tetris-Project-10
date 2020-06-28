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
  //var vmWriter: java.io.PrintWriter = _
  var tokenizing = new Tokenizing
  var parsing = new Parsing
  //var translating = new JACKtoVM
  val help = new HelpFunctions

  var classTable = new SymbolTable
  var methodTable = new SymbolTable

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
    var table = new ListBuffer[SymbolEntry]

    def addRow(name: String, symType: String, segment: String): Unit = {
      var symbol = new SymbolEntry
      if(table.exists(entry => entry.getSegment == segment)){
        val index = table.lastIndexWhere((entry) => entry.getSegment == segment)
        val offset = table.apply(index).getOffset + 1
        symbol.construct(name, symType, segment, offset)
        table.insert(index+1, symbol)
      } else {
        symbol.construct(name, symType, segment, 0)
        table.insert(0, symbol)
      }
    }

    def clearTable(): Unit = {
      table.clear()
    }

    def contains(name: String): Boolean = {
      return table.exists(entry => entry.getName == name)
    }

    def typeOf(name: String): String = {
      val index = table.indexWhere(entry => entry.getName == name)
      return table.apply(index).getType
    }

    def segmentOf(name: String): String = {
      val index = table.indexWhere(entry => entry.getName == name)
      return table.apply(index).getSegment
    }

    def indexOf(name: String): Int = {
      val index = table.indexWhere(entry => entry.getName == name)
      return table.apply(index).getOffset
    }

    def varCount(segment: String): Int = {
      if(table.exists(entry => entry.getSegment == segment)){
        val index = table.lastIndexWhere((entry) => entry.getSegment == segment)
        val offset = table.apply(index).getOffset + 1
        return offset
      } else {
        return 0
      }
    }

    def printTable(): Unit ={
      var num = table.length
      var i = 0
      while(i < num) {
        println(table.apply(i).getName)
        println(table.apply(i).getSegment)
        println(table.apply(i).getType)
        println(table.apply(i).getOffset)
        i+=1
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
      val matcher = """\<.*\>\s(.*?)\s<.*>""".r
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
      xmlWriter.write(s"function ${className}.${subName} ${methodTable.varCount("local")}\n")
      subType match {
        case "constructor" =>
          xmlWriter.write(s"push constant ${classTable.varCount("field")}\n" +
            "call Memory.alloc 1\n" +
            "pop pointer 0\n")
        case "method" =>
          xmlWriter.write("push argument 0\n" +
            "pop pointer 0\n")
        case _ =>
      }
    }

    def writeTable(t: SymbolTable): Unit = {
      t.printTable()
    }

    def getTag(token: String): String = {
      val matcher = """\<.*?\>\s(.*)\s\<.*?\>""".r
      matcher findFirstIn  token match {
        case Some(matcher(outside)) => outside
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

    var IfLabel = 0
    var WhileLabel = 0
    var codeToWrite =""
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
      //classTable.clearTable()

      /*help.writeFormatted("<class>")
      indentLevel += 1
      help.writeFormatted(tokensList(indexOfToken)) //<keyword> class </keyword>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<identifier> Main </identifier>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<symbol> { </symbol>
      indexOfToken += 1*/

      //<keyword> class </keyword>
      indexOfToken += 1
      //<identifier> Main </identifier>
      className = help.getTagContent(tokensList(indexOfToken))
      indexOfToken += 1
      //<symbol> { </symbol>
      indexOfToken += 1


      classVarDeclaration()
      while (/*tokensList(indexOfToken) != null  &&*/ subOpenings.indexOf(help.getTagContent(tokensList(indexOfToken))) >= 0)
        subroutine()

      /*help.writeFormatted(tokensList(indexOfToken)) //<symbol> } </symbol>
      indexOfToken += 1
      indentLevel -= 1
      help.writeFormatted("</class>")
      indexOfToken += 1*/

      //<symbol> } </symbol>
      indexOfToken += 1
      indexOfToken += 1


      help.writeTable(classTable)

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
        //indexOfToken += 1
        //indentLevel -= 1
        //help.writeFormatted("</classVarDec>")

        //<symbol> ; </symbol>
        indexOfToken += 1
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

      //<keyword> 'constructor', 'function', or 'method' </keyword>
      subType = help.getTagContent(tokensList(indexOfToken))
      indexOfToken += 1
      //<keyword>void</keyword>
      indexOfToken += 1
      //<identifier>main</identifier>
      subName = help.getTagContent(tokensList(indexOfToken))
      indexOfToken += 1
      //<symbol>(</symbol>
      indexOfToken += 1

      // if the subroutine is a method - send a copy of the object to the method table
      if(subType == "method")
        methodTable.addRow("this", className, "argument")

      if (help.getTagContent(tokensList(indexOfToken)) != ")")
        subParameters()

      /*help.writeFormatted("</parameterList>")

      help.writeFormatted(tokensList(indexOfToken)) //<symbol>)</symbol>
      indexOfToken += 1*/

      //<symbol>)</symbol>
      indexOfToken += 1

      //help.writeFormatted("<subroutineBody>")
      //indentLevel += 1
      //help.writeFormatted(tokensList(indexOfToken)) //<symbol>{</symbol>
      //indexOfToken += 1

      //<symbol> { </symbol>
      indexOfToken += 1

      varDeclaration()

      help.writeSubOpening() // writing the beginning of the subroutine

      help.writeTable(methodTable)

      statements()

      //help.writeFormatted(tokensList(indexOfToken)) //<symbol>}</symbol>
      //indexOfToken += 1
      /*indentLevel -= 1
      help.writeFormatted("</subroutineBody>")
      indentLevel -= 1
      help.writeFormatted("</subroutineDec>")*/

      //<symbol> } </symbol>
      indexOfToken += 1
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
      //<keyword> int </keyword>
      someType = help.getTagContent(tokensList(indexOfToken))
      indexOfToken += 1
      //<identifier> x </identifier>
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

      //<symbol> , </symbol>
      indexOfToken += 1
      //<keyword> int </keyword>
      someType = help.getTagContent(tokensList(indexOfToken))
      indexOfToken += 1
      //<identifier> x </identifier>
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

        //<keyword> var </keyword>
        someSegment = "local"
        indexOfToken += 1
        //<keyword> int </keyword>
        someType = help.getTagContent(tokensList(indexOfToken))
        indexOfToken += 1
        //<identifier> x </identifier>
        someName = help.getTagContent(tokensList(indexOfToken))
        indexOfToken += 1

        methodTable.addRow(someName, someType, someSegment)

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

          methodTable.addRow(someName, someType, someSegment)
        }
        //help.writeFormatted(tokensList(indexOfToken)) // <symbol> ; </symbol>
        //indexOfToken += 1
       /* indentLevel -= 1
        help.writeFormatted("</varDec>")*/

        // <symbol> ; </symbol>
        indexOfToken += 1
      }
    }

    /**
     *
     */
    def statements(): Unit = {
      /*help.writeFormatted("<statements>")
      indentLevel += 1*/

      while (statStarts.indexOf(help.getTagContent(tokensList(indexOfToken))) >= 0) {
        codeToWrite = ""
        statement()
      }

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
      var varName = ""
      var varSegment = ""

      /*help.writeFormatted("<letStatement>")
      indentLevel += 1
      help.writeFormatted(tokensList(indexOfToken)) //<keyword> let </keyword>
      indexOfToken += 1
      help.writeFormatted(tokensList(indexOfToken)) //<keyword> game </keyword>
      indexOfToken += 1*/

      //<keyword> let </keyword>
      indexOfToken += 1

      var isClass :Boolean = false
      varName = help.getTagContent(tokensList(indexOfToken))
      if(classTable.contains(varName)) {
        isClass = true
        varSegment = classTable.segmentOf(varName)
      } else {
        varSegment = methodTable.segmentOf(varName)
      }

      //<keyword> game </keyword>
      indexOfToken += 1


      if (help.getTagContent(tokensList(indexOfToken)) == "[") {
        /*help.writeFormatted(tokensList(indexOfToken)) //<symbol> [ </symbol>
        indexOfToken += 1
        expression()
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> ] </symbol>
        indexOfToken += 1*/

        //<symbol> [ </symbol>
        indexOfToken += 1

        expression()
        if (isClass) {
          varSegment match {
            case "field" =>
              codeToWrite += s"push this ${classTable.indexOf(varName)}\n"
            case "static" =>
              codeToWrite += s"push static ${classTable.indexOf(varName)}\n"
            case _ =>
              codeToWrite += s"push ${varSegment} ${classTable.indexOf(varName)}\n"
          }
        }
        else {
          varSegment match {
            case "field" =>
              codeToWrite += s"push this ${methodTable.indexOf(varName)}\n"
            case "static" =>
              codeToWrite += s"push static ${methodTable.indexOf(varName)}\n"
            case _ =>
              codeToWrite += s"push ${varSegment} ${methodTable.indexOf(varName)}\n"
          }
        }
        codeToWrite += "add\n"

        //<symbol> ] </symbol>
        indexOfToken += 1

        //<symbol> = </symbol>
        indexOfToken += 1

        expression()
        codeToWrite += "pop temp 0\n" +
          "pop pointer 1\n" +
          "push temp 0\n" +
          "pop that 0\n"

        //<symbol> ; </symbol>
        indexOfToken += 1

      }
      else {
        /*help.writeFormatted(tokensList(indexOfToken)) //<symbol> = </symbol>
      indexOfToken += 1
      expression()
      help.writeFormatted(tokensList(indexOfToken)) //<symbol> ; </symbol>
      indexOfToken += 1
      indentLevel -= 1
      help.writeFormatted("</letStatement>")*/

        //<symbol> = </symbol>
        indexOfToken += 1

        expression()

        //<symbol> ; </symbol>
        indexOfToken += 1

        if (isClass) {
          varSegment match {
            case "field" =>
              codeToWrite += s"pop this ${classTable.indexOf(varName)}\n"
            case "static" =>
              codeToWrite += s"pop static ${classTable.indexOf(varName)}\n"
            case _ =>
              codeToWrite += s"pop ${varSegment} ${classTable.indexOf(varName)}\n"
          }
        } else {
          varSegment match {
            case "field" =>
              codeToWrite += s"pop this ${methodTable.indexOf(varName)}\n"
            case "static" =>
              codeToWrite += s"pop static ${methodTable.indexOf(varName)}\n"
            case _ =>
              codeToWrite += s"pop ${varSegment} ${methodTable.indexOf(varName)}\n"
          }
        }
      }

      xmlWriter.write(codeToWrite)
      codeToWrite = ""
    }

    /**
     *
     */
    def returnStatement(): Unit = {
      /*help.writeFormatted("<returnStatement>")
      indentLevel += 1
      help.writeFormatted(tokensList(indexOfToken)) //<keyword> return </keyword>
      indexOfToken += 1*/

      //<keyword> return </keyword>
      indexOfToken += 1

      if (help.getTagContent(tokensList(indexOfToken)) != ";") {
        expression()
      } else {
        codeToWrite += "push constant 0\n"
      }

      /*help.writeFormatted(tokensList(indexOfToken)) //<symbol> ; </symbol>
      indexOfToken += 1
      indentLevel -= 1
      help.writeFormatted("</returnStatement>")*/

      codeToWrite += "return\n"
      //<symbol> ; </symbol>
      indexOfToken += 1

      xmlWriter.write(codeToWrite)
      codeToWrite = ""
    }

    /**
     *
     */
    def ifStatement(): Unit = {

      /*help.writeFormatted("<ifStatement>")
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
      indexOfToken += 1*/

      indexOfToken += 1
      indexOfToken += 1
      expression()

      codeToWrite += s"if-goto IF_TRUE${IfLabel}\n" +
        s"goto IF_FALSE${IfLabel}\n" +
        s"label IF_TRUE${IfLabel}\n"
      xmlWriter.write(codeToWrite)
      codeToWrite = ""

      indexOfToken += 1
      indexOfToken += 1
      IfLabel += 1
      statements()
      indexOfToken += 1

      if (help.getTagContent(tokensList(indexOfToken)) == "else") {
        /*help.writeFormatted(tokensList(indexOfToken)) //<keyword> else </keyword>
        indexOfToken += 1
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> { </symbol>
        indexOfToken += 1
        statements()
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> } </symbol>
        indexOfToken += 1*/
        IfLabel -= 1
        codeToWrite += s"goto IF_END${IfLabel}\n" +
          s"label IF_FALSE${IfLabel}\n"
        IfLabel += 1

        xmlWriter.write(codeToWrite)
        codeToWrite = ""

        indexOfToken += 1
        indexOfToken += 1
        IfLabel += 1
        statements()
        indexOfToken += 1

        IfLabel -= 2
        codeToWrite += s"label IF_END${IfLabel}\n"
      } else {
        IfLabel -= 1
        codeToWrite += s"label IF_FALSE${IfLabel}\n"
      }
      /*indentLevel -= 1
      help.writeFormatted("</ifStatement>")*/

      xmlWriter.write(codeToWrite)
      codeToWrite = ""
    }

    /**
     *
     */
    def whileStatement(): Unit = {
      /*help.writeFormatted("<whileStatement>")
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
      help.writeFormatted("</whileStatement>")*/

      indexOfToken += 1
      indexOfToken += 1
      expression()
      indexOfToken += 1
      indexOfToken += 1
      statements()
      indexOfToken += 1

      xmlWriter.write(codeToWrite)
      codeToWrite = ""
    }

    /**
     *
     */
    def doStatement(): Unit = {
      /*help.writeFormatted("<doStatement>")
      indentLevel += 1
      help.writeFormatted(tokensList(indexOfToken)) //<keyword> do </keyword>
      indexOfToken += 1*/

      //<keyword> do </keyword>
      indexOfToken += 1

      subroutineCall()
      codeToWrite += "pop temp 0\n"

      //<symbol> ; </symbol>
      indexOfToken += 1

      xmlWriter.write(codeToWrite)
      codeToWrite = ""
      /*help.writeFormatted(tokensList(indexOfToken)) //<symbol> ; </symbol>
            indexOfToken += 1
            indentLevel -= 1
            help.writeFormatted("</doStatement>")*/
    }

    /**
     *
     */
    def expression(): Unit = {
      /*help.writeFormatted("<expression>")
      indentLevel += 1
      term()*/

      term()
      while (opList.indexOf(help.getTagContent(tokensList(indexOfToken))) >= 0) {
        /*help.writeFormatted(tokensList(indexOfToken)) //<symbol> + </symbol>
        indexOfToken += 1
        term()*/

        //<symbol> + </symbol>
        val op = help.getTagContent(tokensList(indexOfToken))
        indexOfToken += 1
        term()

        op match {
          case "+" =>
            codeToWrite += "add\n"
          case "-" =>
            codeToWrite += "sub\n"
          case "*" =>
            codeToWrite += "call Math.multiply 2\n"
          case "/" =>
            codeToWrite += "call Math.divide 2\n"
          case "&amp;" =>
            codeToWrite += "and\n"
          case "|" =>
            codeToWrite += "or\n"
          case "&lt;" =>
            codeToWrite += "lt\n"
          case "&gt;" =>
            codeToWrite += "gt\n"
          case "=" =>
            codeToWrite += "eq\n"
        }
      }
      /*indentLevel -= 1
      help.writeFormatted("</expression>")*/
    }

    /**
     *
     */
    def subroutineCall(): Unit = {
      /*help.writeFormatted(tokensList(indexOfToken)) //<identifier>SquareGame</identifier>
      indexOfToken += 1*/

      var numOfExp = 0
      var subCall = help.getTagContent(tokensList(indexOfToken))
      indexOfToken += 1

      if (help.getTagContent(tokensList(indexOfToken)) == "(") {
        /*help.writeFormatted(tokensList(indexOfToken)) //<symbol> ( </symbol>
        indexOfToken += 1
        expressionList()
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> ) </symbol>
        indexOfToken += 1*/

        codeToWrite += "push pointer 0\n"

        //<symbol> ( </symbol>
        indexOfToken += 1

        numOfExp = expressionList() + 1

        //<symbol> ) </symbol>
        indexOfToken += 1
        codeToWrite += s"call ${className}.${subCall} ${numOfExp}\n"


      } else {
        /*help.writeFormatted(tokensList(indexOfToken)) //<symbol> . </symbol>
        indexOfToken += 1
        help.writeFormatted(tokensList(indexOfToken)) //<identifier>SquareGame</identifier>
        indexOfToken += 1
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> ( </symbol>
        indexOfToken += 1
        expressionList()
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> ) </symbol>
        indexOfToken += 1*/

        //<symbol> . </symbol>
        indexOfToken += 1

        val subCallType = subCall
        subCall = help.getTagContent(tokensList(indexOfToken))

        //<identifier> SquareGame </identifier>
        indexOfToken += 1

        if(methodTable.contains(subCallType)){
          codeToWrite += s"push ${methodTable.segmentOf(subCallType)} ${methodTable.indexOf(subCallType)}\n" // push local 0
          numOfExp = 1
        } else if(subType == "method" && (classTable.contains(subCallType) || (methodTable.contains(subCallType)))) {
          codeToWrite += s"push ${classTable.segmentOf(subCallType)} ${classTable.indexOf(subCallType)}\n" // push local 0
          numOfExp = 1
        }

        //<symbol> ( </symbol>
        indexOfToken += 1
        numOfExp += expressionList()

        //<symbol> ) </symbol>
        indexOfToken += 1
        codeToWrite += s"call ${subCallType}.${subCall} ${numOfExp}\n"
      }
    }

    /**
     * recursive
     */
    def term(): Unit = {
      var varName = ""
      var varSegment = ""
      var varOffset = 0

      val keywordConstantList = List("true", "false", "null", "this")
      /*help.writeFormatted("<term>")
      indentLevel += 1*/

      /*var b = help.getTagContent(tokensList(indexOfToken))
      var c = help.getTokenType(b)
      var d = help.isStringConstant(b)
      var e = (tokensList(indexOfToken)).startsWith("<stringConstant>")*/

      if (help.getTagContent(tokensList(indexOfToken)) == "(") {
        //help.writeFormatted(tokensList(indexOfToken)) //<symbol> ( </symbol>
        /*indexOfToken += 1
        expression()
        //help.writeFormatted(tokensList(indexOfToken)) //<symbol> ) </symbol>
        indexOfToken += 1*/

        //<symbol> ( </symbol>
        indexOfToken += 1
        expression()

        //<symbol> ) </symbol>
        indexOfToken += 1
      }
      else if (help.getTagContent(tokensList(indexOfToken + 1)) == "[") {
        /*help.writeFormatted(tokensList(indexOfToken)) //<symbol> varName </symbol>
        indexOfToken += 1
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> [ </symbol>
        indexOfToken += 1
        expression()
        help.writeFormatted(tokensList(indexOfToken)) //<symbol> ] </symbol>
        indexOfToken += 1*/

        varName = help.getTagContent(tokensList(indexOfToken))
        if(classTable.contains(varName)) {
          varSegment = classTable.segmentOf(varName)
          varOffset = classTable.indexOf(varName)
        } else {
          varSegment = methodTable.segmentOf(varName)
          varOffset = methodTable.indexOf(varName)
        }

        //<symbol> varName </symbol>
        indexOfToken += 1
        //<symbol> [ </symbol>
        indexOfToken += 1
        expression()
        codeToWrite += s"push ${varSegment} ${varOffset}\n" +
          "add\n" +
          "pop pointer 1\n" +
          "push that 0\n"

        //<symbol> ] </symbol>
        indexOfToken += 1
      }
      else if ((help.getTagContent(tokensList(indexOfToken)) == "-") || (help.getTagContent(tokensList(indexOfToken)) == "~")) {
        //help.writeFormatted(tokensList(indexOfToken)) //<symbol> unary op </symbol>

        val op = help.getTagContent(tokensList(indexOfToken))
        //<symbol> unary op </symbol>
        indexOfToken += 1
        term()
        op match {
          case "-" =>
            codeToWrite += "neg\n"
          case "~" =>
            codeToWrite += "not\n"
        }
      }
      else if ((help.getTagContent(tokensList(indexOfToken + 1)) == "(") || (help.getTagContent(tokensList(indexOfToken + 1)) == ".")) {
        subroutineCall()
      }
      else if (help.isIntegerConstant(help.getTagContent(tokensList(indexOfToken)))) {
        varName = help.getTagContent(tokensList(indexOfToken))

        //<IntegerConstant> integerConstant </IntegerConstant>
        indexOfToken += 1

        codeToWrite += s"push constant ${varName}\n"
      }
      else if (tokensList(indexOfToken).startsWith("<stringConstant>")) {
        val varString = help.getTagContent(tokensList(indexOfToken)).map(_.toByte)
        val num = varString.length

        codeToWrite += s"push constant ${num}\n" +
          "call String.new 1\n"
        varString.foreach { x =>
          codeToWrite += s"push constant ${x}\n" +
            "call String.appendChar 2\n"
        }

        //<StringConstant> string </StringConstant>
        indexOfToken += 1
      }
      else if (keywordConstantList.indexOf(help.getTagContent(tokensList(indexOfToken))) >= 0) {
        varName = help.getTagContent(tokensList(indexOfToken))
        varName match {
          case "true" =>
            codeToWrite += "push constant 0 \n" +
              "not \n"
          case "false" =>
            codeToWrite += "push constant 0 \n"
          case "null" =>
            codeToWrite += "push constant 0 \n"
          case "this" =>
            codeToWrite += "push pointer 0 \n"
        }
        indexOfToken += 1
      }
      else {
        //help.writeFormatted(tokensList(indexOfToken)) //<identifier>  </identifier>

        //<identifier>  </identifier>
        varName = help.getTagContent(tokensList(indexOfToken))
        if(classTable.contains(varName)) {
          varSegment = classTable.segmentOf(varName)
          varOffset = classTable.indexOf(varName)
        } else {
          varSegment = methodTable.segmentOf(varName)
          varOffset = methodTable.indexOf(varName)
        }

        indexOfToken += 1

        if(varSegment == "field") {
          varSegment = "this"
        }
        codeToWrite += s"push ${varSegment} ${varOffset}\n"
      }
      /*indentLevel -= 1
      help.writeFormatted("</term>")*/

    }

    /**
     *
     */
    def expressionList(): Int = {
      /*help.writeFormatted("<expressionList>")
      indentLevel += 1*/

      var numOfExp = 0
      if (help.getTagContent(tokensList(indexOfToken)) != ")") {
        numOfExp += 1
        expression()
        while (help.getTagContent(tokensList(indexOfToken)) == ",") {
          //help.writeFormatted(tokensList(indexOfToken)) //<symbol> , </symbol>

          //<symbol> , </symbol>
          indexOfToken += 1
          numOfExp += 1
          expression()
        }
      }
      /*indentLevel -= 1
      help.writeFormatted("</expressionList>")*/

      return numOfExp
    }
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

          // parser creates .vm file
          var strFileName :String = file.getName.replace(".jack",".vm")
          xmlWriter = new PrintWriter(new File(path + strFileName))
          parsing.parser(path + file.getName)
          xmlWriter.close()

          // translator creates .vm file
          /*strFileName = file.getName.replace(".jack",".vm")
          vmWriter = new PrintWriter(new File(path + strFileName))
          translating.translate(path + file.getName)
          vmWriter.close()*/
        }
        else {
          println("Not A JACK File\n")
        }
      }
    }
  }
}

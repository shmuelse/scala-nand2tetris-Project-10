package Parser
import java.io.{File, PrintWriter}

import scala.io.Source
import Utils.Utils



  abstract class Parsing {

    def getContent(str: String, tokenType: String): String

    def writeFormatted(str: String, indentLevel: Int)


  }


  class XMLParsing extends Parsing {

    var helper = new Utils
    var xmlWriter: java.io.PrintWriter = null

    //var tokenHelp = new Tokenizing
    override def getContent(str: String, tokenType: String): String = {
      var lString = str;

      if (tokenType == "stringConstant") {
        lString = lString.drop(1)
        lString = lString.dropRight(1)
      }
      lString match {
        case "<" => return "&lt;"
        case ">" => return "&gt;"
        case "&" => return "&amp;"
        case "'" => return "&apos;"
        case """""" => return "&quot;"
        case _ => return lString
      }

    }

    //return the type of the token ready for XML file
    def writeXmlToken(str: String): String = {
      val tokenName: String = helper.getTokenType(str)
      return ("<" + tokenName + "> " + getContent(str, tokenName) + " </" + tokenName + ">")
    }

    // Create the Xml file
    def createXMLFile(fileName: String) = {
      var isComment: Boolean = false;

      //val fileNameStr:String = fileName.replace(".jack","T.xml")
      val writer = new PrintWriter(new File(fileName.replace(".jack", "T.xml")))

      val delimiterReg = """(?:\/\/.*|\/\*|\*\/|\<|\>|\.|#|&|\,|:|\*|\(|\)|=|\{|\}|\(|\)|\[|\]|\.|\;|\+|\-|\*|\/|\&|\|\|\=|\~|\"[^\"]*\"|\d+\.{0,1}\d*|\s|\n|\w+)?""".r

      writer.write("<tokens>" + "\n")

      Source.fromFile(fileName).getLines().foreach {
        line => {
          val items = delimiterReg findAllIn line
          items.foreach { x =>
            if (x == "/*") isComment = true;
            if (!isComment && x.trim != "" && !helper.isCommentLine(x)) {
              writer.write(writeXmlToken(x) + "\n")
            }
            if (x == "*/") isComment = false
          }
        }
      }
      writer.write("</tokkens" + "\n")
      writer.close()
    }

    // Print to the xml file
    override def writeFormatted(str: String, indentLevel: Int): Unit = {
      xmlWriter.write("  " * indentLevel + str + "\n")
    }



}
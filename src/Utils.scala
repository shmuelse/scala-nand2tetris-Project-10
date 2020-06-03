package Utils

object EX_04 {

  class Utils {


    // ***************** Check Functions ******************** //

    def isIntegerConstant(x: String) = x forall Character.isDigit

    def isStringConstant(x: String) = x.matches("""^\"[^\\"]*\"$""")

    def isIdentifier(x: String) = x.matches("""^[^\d][\d\w\_]*""")

    def isCommentLine(x: String) = x.matches("""^\/\/.*""")

    def hasJackFileExtention(x: String) = x.matches("^.*\\.jack$")


    // ***************** Help Functions ******************** //


    def getTokenType(tokenType: String): String = {
      val isKeyword = List(
        "Class", "constructor", "function", "method", "field", "static", "var", "int", "char", "boolean", "void", "true", "false"
        , "null", "this", "let", "do", "if", "else", "while", "return")

      val isSymbol = List("{", "}", "(", ")", "[", "]", ".", ",", ";", "+", "-", "*", "/", "&", "", "", "<", ">", "=", "~")

      if (isKeyword.indexOf(tokenType) >= 0)
        return "keyword"
      if (isSymbol.indexOf(tokenType) >= 0)
        return "symbol"
      if (isIntegerConstant(tokenType))
        return "integerConstant"
      if (isStringConstant(tokenType))
        return "stringConstant"
      if (isIdentifier(tokenType))
        return "identifier"
      return ""
    }

    def getTagContent(token: String): String = {
      val matcher = """\<.*\>\s(.*?)\s\<.*\>""".r
      matcher findFirstIn token match {
        case Some(matcher(inside)) => return inside
        case _ => return ""
      }
    }


  }

}
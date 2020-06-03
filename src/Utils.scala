object EX_04 {

  class Utils {

    def isIntegerConstant(x: String) = x forall Character.isDigit

    def isStringConstant(x: String) = x.matches("""^\"[^\\"]*\"$""")

    def isIdentifier(x: String) = x.matches("""^[^\d][\d\w\_]*""")

    def isCommentLine(x: String) = x.matches("""^\/\/.*""")

    //Function to check the type of the token
    def getTokenType (tokenType: String):String = {
      val isKeyword = List(
        "Class", "constructor","function","method","field","static","var","int","char","boolean","void","true","false"
        ,"null","this","let","do","if","else","while","return")

      val isSymbol = List("{", "}", "(", ")", "[", "]", ".", ",", ";", "+", "-", "*", "/", "&", "", "", "<", ">", "=", "~")

      if(isKeyword.indexOf(tokenType) >= 0)
        return "keyword"
      if(isSymbol.indexOf(tokenType) >= 0)
        return "symbol"
      if(isIntegerConstant(tokenType))
        return "integerConstant"
      if(isStringConstant(tokenType))
        return "stringConstant"
      if(isIdentifier(tokenType))
        return "identifier"
      return ""
    }




  }

}
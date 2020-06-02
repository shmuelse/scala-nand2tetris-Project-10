import java.io.File

import token.Tokenizing
import scala.io.Source

class helpFunctions() {
  def hasJACKFileExtention(x: String) = x.matches("^.*\\.jack$")
}

object main extends App {
  println("Enter file path:")

  val read = scala.io.StdIn.readLine()
  val path = new java.io.File(read).getCanonicalPath
  println("path is:\n" + path)


  val tok = new Tokenizing()
  var func = new helpFunctions()
  var str = ""
    refArrayOps(new File(path).listFiles).foreach{
      file => {
        //val beginValue = file.getPath().lastIndexOf("\\")
        //val beginName = file.getName.substring(beginValue+1, path.length)


        if(!func.hasJACKFileExtention(file.getPath)) {
          println("Not A JACK File\n")
        } else {
          val jackFileName = file.getName
          val fileName = jackFileName.replaceAll(".jack", "")

          val lines = Source.fromFile(file.getPath).mkString //.replaceAll(" ", "")

          println("the file is:\n" + fileName)
          tok.tokenize(fileName, path, lines)
        }
      }
    }
}

import java.io.File

import Utils.Utils
import Parser.{Parsing, XMLParsing}
import token.Tokenizing

import scala.io.Source



  object main extends App {
    println("Enter file path:")

    val read = scala.io.StdIn.readLine()
    val path = new java.io.File(read).getCanonicalPath
    println("path is:\n" + path)


    val tok = new Tokenizing()
    var func = new Utils
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

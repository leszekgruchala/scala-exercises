package eu.gruchala.apps

import scala.io.{BufferedSource, Source}

object UrlExecutorApp extends App {

  //Given a CSV sourceFile containing one URL per line, write a program
  // that reads those URLs and downloads the contents.
  def readFile(fileName: String): BufferedSource =
    Source.fromFile(fileName, "UTF-8")

  def readUrls(source: BufferedSource): Seq[UrlName] = {
    source.getLines().filter(_.nonEmpty).map(UrlName).toSeq
  }

  case class UrlName(value: String) extends AnyVal
  case class UrlContent(value: String) extends AnyVal
  def invokeUrls(urls: Seq[UrlName]): Seq[UrlContent] = {
    urls.map { url =>
      val call = scala.io.Source.fromURL(url.value, "UTF-8")
      UrlContent(call.getLines().toList.mkString(""))
    }
  }

  def storeFile(fileName: String, urlContent: UrlContent): Unit = {
    def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
      val p = new java.io.PrintWriter(f)
      try { op(p) } finally { p.close() }
    }

    import java.io._
    printToFile(new File(fileName)) { p =>
      p.println(urlContent.value)
    }
  }


  val sourceFile = readFile("urls.csv")
  val urls = readUrls(sourceFile)
  val data = invokeUrls(urls)
  data.zipWithIndex.foreach{ case (content, index) => storeFile(s"$index.txt", content)}
}

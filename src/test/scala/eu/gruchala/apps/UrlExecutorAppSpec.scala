package eu.gruchala.apps

import java.io.FileNotFoundException

import eu.gruchala.BaseSpec

class UrlExecutorAppSpec extends BaseSpec {

  import eu.gruchala.apps.UrlExecutorApp._

  "reading a file" should {
    "return file contents" when {
      "file exists" in {
        val buffer = readFile("urls.csv")
        buffer.toList should not be empty
      }
    }

    "throw an exception" when {
      "file does not exists" in {
        an[FileNotFoundException] shouldBe thrownBy {
          readFile("AAA.csv")
        }
      }
    }
  }
}

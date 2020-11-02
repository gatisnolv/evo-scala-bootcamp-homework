package com.gatis.homework.async

import java.net.URL
import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration._

import scala.io.Source
import scala.concurrent.Await
import scala.util.Success
import scala.util.Failure
import cats.implicits._
import cats.instances._

/**
  * Application:
  * - takes a web-page URL from arguments (args array)
  * - loads the web-page body, extracts HTTP links from it
  * - for all the found links, tries to fetch a server name header if there is one
  * - prints all the encountered unique server name values in alphabetical order
  *
  * Each link processing should be done in parallel.
  * Validation of arguments is not needed.
  *
  * Try to test it on http://google.com!
  */
object AsyncHomework extends App {
  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  //put your code there
  if (args.length > 0) {
    args foreach execute
  } else {
    execute("http://google.com")
  }

  def execute(url: String): Unit = {
    val sortedNamesFuture = (for {
      body <- fetchPageBody(url)
      links <- findLinkUrls(body)
      potentialNames <- Future.traverse(links)(fetchServerName)
    } yield potentialNames.filter(_.isDefined).flatten.toSet.toList.sorted)

    sortedNamesFuture onComplete (_ match {
      case Success(names)     => names foreach println
      case Failure(exception) => println(s"Failed: ${exception}")
    })
  }

  private def fetchPageBody(url: String): Future[String] = {
    println(f"Fetching $url")
    Future {
      val source = Source.fromURL(url)("ISO-8859-1")
      try {
        source.mkString
      } finally {
        source.close()
      }
    }
  }

  private def fetchServerName(url: String): Future[Option[String]] = {
    println(s"Fetching server name header for $url")
    Future {
      Option(new URL(url).openConnection().getHeaderField("Server"))
    }
  }

  private def findLinkUrls(html: String): Future[List[String]] = Future {
    val linkPattern = """href="(http[^"]+)"""".r
    linkPattern.findAllMatchIn(html).map(m => m.group(1)).toList
  }
}

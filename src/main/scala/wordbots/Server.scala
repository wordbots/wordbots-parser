package wordbots

import com.workday.montague.semantics.Form
import org.http4s._
import org.http4s.dsl._
import org.http4s.server.blaze.BlazeBuilder

object Server {
  val service = HttpService {
    case request @ POST -> Root / "parse" =>
      Ok(request.bodyAsText map { input =>
        val result = Parser.parse(input)

        val code = result.bestParse.map(_.semantic) match {
          case Some(Form(v: AstNode)) => CodeGenerator.generateJS(v)
          case _ => "(n/a)"
        }

        code
      })
  }

  val host = "0.0.0.0"
  val port = (Option(System.getenv("PORT")) orElse
    Option(System.getenv("HTTP_PORT")))
    .map(_.toInt)
    .getOrElse(8080)

  def main(args: Array[String]): Unit = {
    println(s"Starting server example on '$host:$port'")

    BlazeBuilder
      .bindHttp(port, host)
      .mountService(service)
      .run
      .awaitShutdown
  }
}
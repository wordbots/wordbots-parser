package wordbots

import com.workday.montague.semantics.Form
import org.http4s._
import org.http4s.dsl._
import org.http4s.server.blaze.BlazeBuilder
import org.log4s.getLogger

class Server(host: String, port: Int) {
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
  // Build the server instance and begin.
  def run(): Unit = BlazeBuilder
    .bindHttp(port, host)
    .mountService(service)
    .run
    .awaitShutdown
}

object Server {
  private val logger = getLogger

  val ip = "0.0.0.0"
  val port = (Option(System.getenv("PORT")) orElse
    Option(System.getenv("HTTP_PORT")))
    .map(_.toInt)
    .getOrElse(8080)

  logger.info(s"Starting Http4s-blaze example on '$ip:$port'")
  println(s"Starting Http4s-blaze example on '$ip:$port'")

  def main(args: Array[String]): Unit = new Server(ip, port).run()
}
package wordbots

import com.workday.montague.semantics.Form
import org.http4s._
import org.http4s.dsl._
import org.http4s.server.{ServerApp, Server => Http4sServer}
import org.http4s.server.blaze._

import scalaz.concurrent.Task

object Server extends ServerApp {
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

  override def server(args: List[String]): Task[Http4sServer] = {
    println(s"Binding to port ${sys.env.getOrElse("PORT", "8080")} ...")

    BlazeBuilder
      .bindHttp(sys.env.getOrElse("PORT", "8080").toInt)
      .mountService(service, "/")
      .start
  }
}
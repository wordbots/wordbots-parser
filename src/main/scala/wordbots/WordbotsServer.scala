package wordbots

import com.workday.montague.semantics.Form
import org.http4s._
import org.http4s.dsl._
import org.http4s.server.{Server, ServerApp}
import org.http4s.server.blaze.BlazeBuilder

import scala.util.{Failure, Success, Try}

import scalaz.concurrent.Task

object WordbotsServer extends ServerApp {
  object InputParamMatcher extends QueryParamDecoderMatcher[String]("input")
  object FormatParamMatcher extends OptionalQueryParamDecoderMatcher[String]("format")

  val service = HttpService {
    case request @ GET -> Root / "parse" :? InputParamMatcher(input) +& FormatParamMatcher(format) =>
      val result = Parser.parse(input)

      format match {
        case Some("js") =>
         result.bestParse.map(_.semantic) match {
            case Some(Form(v: AstNode)) =>
              AstValidator.validate(v) match {
                case Success(_) => Ok(CodeGenerator.generateJS(v))
                case Failure(ex: Throwable) =>
                  InternalServerError("{\"error\": \"" + ex.getMessage + "\"}")
              }
            case _ =>
              val unrecognizedTokens = Parser.findUnrecognizedTokens(input)
              InternalServerError("{\"error\": \"Parse failed\", \"unrecognizedTokens\": [" + unrecognizedTokens.mkString("\"", "\",\"", "\"") +  "]}")
          }

        case Some("svg") =>
          result.bestParse
            .map(parse => Ok(parse.toSvg, Headers(Header("Content-Type", "image/svg+xml"))))
            .getOrElse(InternalServerError("{\"error\": \"Parse failed\"}"))

        case _ => BadRequest("{\"error\": \"Invalid format\"}")
      }
  }

  val host = "0.0.0.0"
  val port = (Option(System.getenv("PORT")) orElse
    Option(System.getenv("HTTP_PORT")))
    .map(_.toInt)
    .getOrElse(8080)

  override def server(args: List[String]): Task[Server] = {
    println(s"Starting server on '$host:$port' ...")

    BlazeBuilder
      .bindHttp(port, host)
      .mountService(service)
      .start
  }
}
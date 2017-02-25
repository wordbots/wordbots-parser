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
                case Success(_) => Ok(CodeGenerator.generateJS(v), headers())
                case Failure(ex: Throwable) =>
                  InternalServerError("{\"error\": \"" + ex.getMessage + "\"}", headers())
              }
            case _ =>
              val unrecognizedTokens = Parser.findUnrecognizedTokens(input)
              InternalServerError(
                "{\"error\": \"Parse failed\", \"unrecognizedTokens\": [" + unrecognizedTokens.mkString("\"", "\",\"", "\"") +  "]}",
                headers()
              )
          }

        case Some("svg") =>
          result.bestParse
            .map(parse => Ok(parse.toSvg, headers(Some("image/svg+xml"))))
            .getOrElse(InternalServerError("{\"error\": \"Parse failed\"}", headers()))

        case _ => BadRequest("{\"error\": \"Invalid format\"}", headers())
      }
  }

  val host = "0.0.0.0"
  val defaultPort = 8080
  val port = (Option(System.getenv("PORT")) orElse
    Option(System.getenv("HTTP_PORT")))
    .map(_.toInt)
    .getOrElse(defaultPort)

  def headers(contentType: Option[String] = None): Headers = {
    contentType match {
      case Some(ct) => Headers(Header("Access-Control-Allow-Origin", "*"), Header("Content-Type", ct))
      case None => Headers(Header("Access-Control-Allow-Origin", "*"))
    }
  }

  override def server(args: List[String]): Task[Server] = {
    // scalastyle:off regex
    println(s"Starting server on '$host:$port' ...")
    // scalastyle:on regex

    BlazeBuilder
      .bindHttp(port, host)
      .mountService(service)
      .start
  }
}

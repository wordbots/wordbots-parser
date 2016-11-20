package wordbots

import com.workday.montague.semantics.Form
import org.http4s._
import org.http4s.dsl._
import org.http4s.server.blaze.BlazeBuilder

object Server {
  object InputParamMatcher extends QueryParamDecoderMatcher[String]("input")
  object FormatParamMatcher extends OptionalQueryParamDecoderMatcher[String]("format")

  val service = HttpService {
    case request @ GET -> Root / "parse" :? InputParamMatcher(input) +& FormatParamMatcher(format) =>
      val result = Parser.parse(input)

      format match {
        case Some("js") =>
         result.bestParse.map(_.semantic) match {
            case Some(Form(v: AstNode)) => Ok(CodeGenerator.generateJS(v))
            case _ =>
              val unrecognizedTokens = Parser.findUnrecognizedTokens(input)
              InternalServerError("{\"error\": \"Parse failed\", \"unrecognizedTokens\": [" + unrecognizedTokens.mkString("\"", "\",\"", "\"") +  "]}")
          }

        case Some("svg") =>
          result.bestParse
            .map(parse => Ok(parse.toSvg))
            .getOrElse(InternalServerError("{\"error\": \"Parse failed\"}"))

        case _ => BadRequest("{\"error\": \"Invalid format\"")
      }
  }

  val host = "0.0.0.0"
  val port = (Option(System.getenv("PORT")) orElse
    Option(System.getenv("HTTP_PORT")))
    .map(_.toInt)
    .getOrElse(8080)

  def main(args: Array[String]): Unit = {
    println(s"Starting server on '$host:$port' ...")

    BlazeBuilder
      .bindHttp(port, host)
      .mountService(service)
      .run
      .awaitShutdown
  }
}
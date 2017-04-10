package wordbots

import com.workday.montague.semantics.Form
import org.http4s._
import org.http4s.dsl._
import org.http4s.server.{Server, ServerApp}
import org.http4s.server.blaze.BlazeBuilder

import scalaz.concurrent.Task

object WordbotsServer extends ServerApp {
  object InputParamMatcher extends QueryParamDecoderMatcher[String]("input")
  object FormatParamMatcher extends OptionalQueryParamDecoderMatcher[String]("format")
  object ModeParamMatcher extends OptionalQueryParamDecoderMatcher[String]("mode")

  val service = {
    HttpService {
      case request @ GET -> Root / "parse" :? InputParamMatcher(input) +& FormatParamMatcher(format) +& ModeParamMatcher(mode) =>
        implicit val validationMode = mode match {
          case Some("object") => ValidateObject
          case Some("event") => ValidateEvent
          case _ => ValidateUnknownCard
        }

        val result = Parser.parse(input).bestParse
        val unrecognizedTokens = Parser.findUnrecognizedTokens(input).mkString("\"", "\",\"", "\"")

        format match {
          case Some("js") =>
            Parser.diagnoseError(input, result) match {
              case Some(error) => errorResponse(error, unrecognizedTokens)
              case None =>
                result.map(_.semantic) match {
                  case Some(Form(v: AstNode)) => successResponse(CodeGenerator.generateJS(v))
                  case _ => errorResponse("Unspecified parser error", unrecognizedTokens)
                }
            }

          case Some("svg") =>
            result
              .map(parse => Ok(parse.toSvg, headers(Some("image/svg+xml"))))
              .getOrElse(errorResponse("Parse failed"))

          case _ => BadRequest("{\"error\": \"Invalid format\"}", headers())
        }

      case request @ GET -> Root / "lexicon" :? FormatParamMatcher(format) =>
        val terms: List[String] = Lexicon.lexicon.map.keys.toList

        format match {
          case Some("json") => Ok(terms.sorted.mkString("[\"", "\", \"", "\"]"), headers())
          case None => Ok(s"I can understand ${terms.size.toString} terms:\n\n${terms.sorted.mkString("\n")}", headers())
          case _ => BadRequest("{\"error\": \"Invalid format\"}", headers())
        }
    }
  }

  val host = "0.0.0.0"
  val defaultPort = 8080
  val port = (Option(System.getenv("PORT")) orElse
    Option(System.getenv("HTTP_PORT")))
    .map(_.toInt)
    .getOrElse(defaultPort)

  def successResponse(js: String): Task[Response] = {
    Ok("{\"js\": \"" + js + "\"}", headers())
  }

  def errorResponse(error: String, unrecognizedTokens: String = ""): Task[Response] = {
    Ok("{\"error\": \"" + error + "\", \"unrecognizedTokens\": [" + unrecognizedTokens +  "]}", headers())
  }

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

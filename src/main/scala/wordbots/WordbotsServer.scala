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

  lazy val lexicon: Map[String, Seq[(String, String)]] = {
    Lexicon.lexicon.map
      .mapValues(defs => defs.map { case (syn, sem) =>
        (
          syn.toString
            .replaceAllLiterally("Noun", "N")
            .replaceAllLiterally("\\", "\\\\"),
          sem.toString
            .replaceAllLiterally("Lambda", "λ ")
            .replaceAllLiterally("\\", "\\\\")
            .replaceAllLiterally("\"", "\\\"")
            .replaceAllLiterally("\n", " ")
        )
      })
  }

  lazy val lexiconTerms: List[String] = lexicon.keys.toList.sorted

  lazy val lexiconJson: String = {
    lexicon
      .filterKeys(term => term != "\"" && term != "'")
      .map { case (term, defs) =>
        "\"" + term + "\": " + defs.map {
          case (syn, sem) => "{\"syntax\": \"" + syn + "\", \"semantics\": \"" + sem + "\"}"
        }.mkString("[", ", ", "]")
      }
      .mkString("{", ", ", "}")
  }

  val service = {
    HttpService {
      case request @ GET -> Root / "parse" :? InputParamMatcher(input) +& FormatParamMatcher(format) +& ModeParamMatcher(mode) =>
        implicit val validationMode = mode match {
          case Some("object") => ValidateObject
          case Some("event") => ValidateEvent
          case _ => ValidateUnknownCard
        }

        val result = Parser.parse(input).bestParse
        val parsedTokens = {
          result.toSeq
            .flatMap(_.terminals)
            .flatMap(_.parseTokens)
            .map(_.tokenString)
            .filter(token => lexiconTerms.contains(token) && token != "\"")
            .mkString("\"", "\",\"", "\"")
        }
        val unrecognizedTokens = Parser.findUnrecognizedTokens(input).mkString("\"", "\",\"", "\"")

        format match {
          case Some("js") =>
            Parser.diagnoseError(input, result) match {
              case Some(error) => errorResponse(error, unrecognizedTokens)
              case None =>
                result.map(_.semantic) match {
                  case Some(Form(v: AstNode)) => successResponse(CodeGenerator.generateJS(v), parsedTokens)
                  case _ => errorResponse(ParserError("Unspecified parser error"), unrecognizedTokens)
                }
            }

          case Some("svg") =>
            result
              .map(parse => Ok(parse.toSvg, headers(Some("image/svg+xml"))))
              .getOrElse(errorResponse())

          case _ => BadRequest("{\"error\": \"Invalid format\"}", headers())
        }

      case request @ GET -> Root / "lexicon" :? FormatParamMatcher(format) =>
        format match {
          case Some("json") => Ok(lexiconJson, headers())
          case None => Ok(s"I can understand ${lexicon.size.toString} terms:\n\n${lexiconTerms.mkString("\n")}", headers())
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

  def successResponse(js: String, parsedTokens: String = ""): Task[Response] = {
    Ok("{\"js\": \"" + js + "\", \"tokens\": [" + parsedTokens +  "]}", headers())
  }

  def errorResponse(error: ParserError = ParserError("Parse failed"), unrecognizedTokens: String = ""): Task[Response] = {
    Ok(
      "{\"error\": \"" + error.description + "\", " +
       "\"suggestions\": [" + error.suggestions.map(s => "\"" + s + "\"").mkString(", ") + "], " +
       "\"unrecognizedTokens\": [" + unrecognizedTokens +  "]}",
      headers()
    )
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

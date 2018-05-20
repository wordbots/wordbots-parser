package wordbots

import com.workday.montague.ccg.CcgCat
import com.workday.montague.parser.SemanticParseNode
import com.workday.montague.semantics.Form
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl._
import org.http4s.server.{Server, ServerApp}
import org.http4s.server.blaze.BlazeBuilder
import io.circe._

import scalaz.concurrent.Task

case class ParseRequest(input: String, mode: String)

sealed trait ParserResponse
case class SuccessfulParse(parse: SemanticParseNode[CcgCat], ast: AstNode, parsedTokens: Seq[String]) extends ParserResponse
case class FailedParse(error: ParserError, unrecognizedTokens: Seq[String]) extends ParserResponse

object WordbotsServer extends ServerApp {
  object InputParamMatcher extends QueryParamDecoderMatcher[String]("input")
  object FormatParamMatcher extends OptionalQueryParamDecoderMatcher[String]("format")
  object ModeParamMatcher extends OptionalQueryParamDecoderMatcher[String]("mode")

  val host = "0.0.0.0"
  val defaultPort = 8080
  val port = (Option(System.getenv("PORT")) orElse Option(System.getenv("HTTP_PORT"))).map(_.toInt).getOrElse(defaultPort)

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
      case request @ OPTIONS -> Root / "parse" =>
        Ok("", headers())

      case request @ GET -> Root / "parse" :? InputParamMatcher(input) +& FormatParamMatcher(format) +& ModeParamMatcher(mode) =>
        parse(input, mode) match {
          case SuccessfulParse(parse, ast, parsedTokens) =>
            format match {
              case Some("js") => successResponse(CodeGenerator.generateJS(ast), parsedTokens)
              case Some("svg") => Ok(parse.toSvg, headers(Some("image/svg+xml")))
              case _ => BadRequest("{\"error\": \"Invalid format\"}", headers())
            }
          case FailedParse(error, unrecognizedTokens) => errorResponse(error, unrecognizedTokens)
        }

      case request @ POST -> Root / "parse" =>
        implicit val parseRequestDecoder: Decoder[ParseRequest] = Decoder.forProduct2("input", "mode")(ParseRequest.apply)

        request.as(jsonOf[Seq[ParseRequest]]).flatMap { parseRequests: Seq[ParseRequest] =>
          val responseBody: String = parseRequests.map { req: ParseRequest =>
            val responseJson = parse(req.input, Option(req.mode)) match {
              case SuccessfulParse(_, ast, parsedTokens) => successResponseJson(CodeGenerator.generateJS(ast), parsedTokens)
              case FailedParse(error, unrecognizedTokens) => errorResponseJson(error, unrecognizedTokens)
            }
            s""""${req.input.replaceAllLiterally("\"", "\\\"")}": $responseJson"""
          }.mkString("{", ",", "}")
          Ok(responseBody, headers())
        }

      case request @ GET -> Root / "lexicon" :? FormatParamMatcher(format) =>
        format match {
          case Some("json") => Ok(lexiconJson, headers())
          case None => Ok(s"I can understand ${lexicon.size.toString} terms:\n\n${lexiconTerms.mkString("\n")}", headers())
          case _ => BadRequest("{\"error\": \"Invalid format\"}", headers())
        }
    }
  }

  def parse(input: String, mode: Option[String]): ParserResponse = {
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
    }
    val unrecognizedTokens = ErrorAnalyzer.findUnrecognizedTokens(input)

    ErrorAnalyzer.diagnoseError(input, result) match {
      case Some(error) => FailedParse(error, unrecognizedTokens)
      case None =>
        result.map(_.semantic) match {
          case Some(Form(v: AstNode)) => SuccessfulParse(result.get, v, parsedTokens)
          case _ => FailedParse(ParserError("Unspecified parser error"), unrecognizedTokens)
        }
    }
  }

  def successResponseJson(js: String, parsedTokens: Seq[String] = Seq()): String = {
    "{\"js\": \"" + js + "\", \"tokens\": [" + parsedTokens.mkString("\"", "\",\"", "\"") + "], \"version\": \"" + Parser.VERSION + "\"}"
  }

  def errorResponseJson(error: ParserError = ParserError("Parse failed"), unrecognizedTokens: Seq[String] = Seq()): String = {
    "{\"error\": \"" + error.description.replaceAllLiterally("\\", "\\\\") + "\", " +
      "\"suggestions\": [" + error.suggestions.map(s => "\"" + s.replaceAllLiterally("\"", "\\\"") + "\"").mkString(", ") + "], " +
      "\"unrecognizedTokens\": [" + unrecognizedTokens.mkString("\"", "\",\"", "\"") + "]}"
  }

  def successResponse(js: String, parsedTokens: Seq[String] = Seq()): Task[Response] = {
    Ok(successResponseJson(js, parsedTokens), headers())
  }

  def errorResponse(error: ParserError = ParserError("Parse failed"), unrecognizedTokens: Seq[String] = Seq()): Task[Response] = {
    Ok(errorResponseJson(error, unrecognizedTokens), headers())
  }

  def headers(contentType: Option[String] = None): Headers = {
    val baseHeaders: List[Header] = List(
      Header("Access-Control-Allow-Origin", "*"),
      Header("Access-Control-Allow-Methods", "GET, POST"),
      Header("Access-Control-Allow-Headers", "Content-Type")
    )

    contentType match {
      case Some(ct) => Headers(baseHeaders :+ Header("Content-Type", ct))
      case None => Headers(baseHeaders)
    }
  }

  override def server(args: List[String]): Task[Server] = {
    // scalastyle:off regex
    println(s"Starting server on '$host:$port' ...")
    // scalastyle:on regex

    //println(s"v${getClass.getPackage.getImplementationVersion.split("-SNAPSHOT")(0)}")

    BlazeBuilder
      .bindHttp(port, host)
      .mountService(service)
      .start
  }
}

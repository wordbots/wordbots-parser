package wordbots

import com.workday.montague.ccg.CcgCat
import com.workday.montague.parser.SemanticParseNode
import com.workday.montague.semantics.{Form, SemanticState}
import org.http4s.{Response => H4sResponse, _}
import org.http4s.circe._
import org.http4s.dsl._
import org.http4s.server.{ServerApp, Server => H4sServer}
import org.http4s.server.blaze.BlazeBuilder
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import scalaz.Memo
import scalaz.concurrent.Task
import wordbots.Semantics.AstNode

import scala.util.{Failure, Success}

object MemoParser {
  sealed trait ParserOutput
  case class SuccessfulParse(parse: SemanticParseNode[CcgCat], ast: Semantics.AstNode, parsedTokens: Seq[String]) extends ParserOutput
  case class FailedParse(error: ParserError, unrecognizedTokens: Seq[String]) extends ParserOutput

  /** Parse the given input string in the given mode,
    * memoizing results along the way,
    * returning a [[ParserOutput]]. */
  def apply(input: String, mode: Option[String]): ParserOutput = {
    // scalastyle:off regex
    print(s"> ${input.trim}")

    val output = parseMemoize(input, mode)
    println()
    output
    // scalastyle:on regex
  }

  private val parseMemoize: ((String, Option[String])) => ParserOutput = {
    Memo.mutableHashMapMemo { args: (String, Option[String]) =>
      parse(args._1, args._2)
    }
  }

  private def parse(input: String, mode: Option[String]): ParserOutput = {
    print("  (not in cache, parsing ...)")

    implicit val validationMode: ValidationMode = mode match {
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
        .filter(token => Lexicon.listOfTerms.contains(token) && token != "\"")
    }
    val unrecognizedTokens = ErrorAnalyzer.findUnrecognizedTokens(input)

    ErrorAnalyzer.diagnoseError(input, result) match {
      case Some(error) => FailedParse(error, unrecognizedTokens)
      case None =>
        result.map(_.semantic) match {
          case Some(Form(ast: AstNode)) => SuccessfulParse(result.get, ast, parsedTokens)
          case _ => FailedParse(ParserError("Unspecified parser error"), unrecognizedTokens)
        }
    }
  }
}

object Server extends ServerApp {
  import MemoParser.{ FailedParse, SuccessfulParse }

  case class ParseRequest(input: String, mode: String)

  sealed trait Response
  case class ErrorResponse(error: String) extends Response
  case class SuccessfulParseResponse(js: String, tokens: Seq[String], version: String = Parser.VERSION) extends Response
  case class FailedParseResponse(error: String, suggestions: Seq[String], unrecognizedTokens: Seq[String]) extends Response
  object FailedParseResponse {
    def apply(error: ParserError = ParserError("Parse failed"), unrecognizedTokens: Seq[String] = Seq()): FailedParseResponse = {
      FailedParseResponse(error.description, error.suggestions.toSeq, unrecognizedTokens)
    }
  }

  object InputParamMatcher extends QueryParamDecoderMatcher[String]("input")
  object FormatParamMatcher extends OptionalQueryParamDecoderMatcher[String]("format")
  object ModeParamMatcher extends OptionalQueryParamDecoderMatcher[String]("mode")

  val host = "0.0.0.0"
  val defaultPort = 8080
  val port: Int = (Option(System.getenv("PORT")) orElse Option(System.getenv("HTTP_PORT"))).map(_.toInt).getOrElse(defaultPort)

  val service: HttpService = HttpService {
    case OPTIONS -> Root / "parse" =>
      Ok("", headers())

    case GET -> Root / "parse" :? InputParamMatcher(input) +& FormatParamMatcher(format) +& ModeParamMatcher(mode) =>
      MemoParser(input, mode) match {
        case SuccessfulParse(parse, ast, parsedTokens) =>
          format match {
            case Some("js") =>
              CodeGenerator.generateJS(ast) match {
                case Success(js: String) => successResponse(js, parsedTokens)
                case Failure(ex: Throwable) => errorResponse(ParserError(s"Invalid JavaScript produced: ${ex.getMessage}. Contact the developers."))
              }
            case Some("svg") => Ok(parse.toSvg, headers(Some("image/svg+xml")))
            case _ => BadRequest(ErrorResponse("Invalid format").asJson, headers())
          }
        case FailedParse(error, unrecognizedTokens) => errorResponse(error, unrecognizedTokens)
      }

    case request @ POST -> Root / "parse" =>
      implicit val parseRequestDecoder: Decoder[ParseRequest] = Decoder.forProduct2("input", "mode")(ParseRequest.apply)

      request.as(jsonOf[Seq[ParseRequest]]).flatMap { parseRequests: Seq[ParseRequest] =>
        val responseBody: Json = parseRequests.map { req: ParseRequest =>
          val parseResponse: Response = MemoParser(req.input, Option(req.mode)) match {
            case SuccessfulParse(_, ast, parsedTokens) =>
              CodeGenerator.generateJS(ast) match {
                case Success(js: String) => SuccessfulParseResponse(js, parsedTokens)
                case Failure(ex: Throwable) => FailedParseResponse(ParserError(s"Invalid JavaScript produced: ${ex.getMessage}. Contact the developers."))
              }
            case FailedParse(error, unrecognizedTokens) => FailedParseResponse(error, unrecognizedTokens)
          }
          req.input -> parseResponse
        }.asJson
        Ok(responseBody, headers())
      }

    case GET -> Root / "lexicon" :? FormatParamMatcher(format) =>
      format match {
        case Some("json") => Ok(Lexicon.asJson, headers())
        case None => Ok(s"I can understand ${Lexicon.listOfTerms.size.toString} terms:\n\n${Lexicon.listOfTerms.mkString("\n")}", headers())
        case _ => BadRequest(ErrorResponse("Invalid format").asJson, headers())
      }
  }

  def successResponse(js: String, parsedTokens: Seq[String] = Seq()): Task[H4sResponse] = {
    Ok(SuccessfulParseResponse(js, parsedTokens).asJson, headers())
  }

  def errorResponse(error: ParserError = ParserError("Parse failed"), unrecognizedTokens: Seq[String] = Seq()): Task[H4sResponse] = {
    Ok(FailedParseResponse(error, unrecognizedTokens).asJson, headers())
  }

  private def headers(contentType: Option[String] = None): Headers = {
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

  override def server(args: List[String]): Task[H4sServer] = {
    // scalastyle:off regex
    println(s"Starting server on '$host:$port' ...")
    // scalastyle:on regex

    BlazeBuilder
      .bindHttp(port, host)
      .mountService(service)
      .start
  }
}

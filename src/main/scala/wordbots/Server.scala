package wordbots

import com.roundeights.hasher.Implicits._
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
  def apply(input: String, mode: Option[String], fastErrorAnalysisMode: Boolean = false): ParserOutput = {
    // scalastyle:off regex
    print(s">${if (fastErrorAnalysisMode) "*" else ""} ${input.trim}")

    val output = parseMemoize(input, mode, fastErrorAnalysisMode)
    println()
    output
    // scalastyle:on regex
  }

  private val parseMemoize: ((String, Option[String], Boolean)) => ParserOutput = {
    Memo.mutableHashMapMemo { args: (String, Option[String], Boolean) =>
      parse(args._1, args._2, args._3)
    }
  }

  private def parse(input: String, mode: Option[String], fastErrorAnalysisMode: Boolean = false): ParserOutput = {
    print(s"  (not in cache, parsing ...)")

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

    ErrorAnalyzer.diagnoseError(input, result, fastErrorAnalysisMode) match {
      case Some(error) =>
        print("  [F]")
        FailedParse(error, unrecognizedTokens)
      case None =>
        result.map(_.semantic) match {
          case Some(Form(ast: AstNode)) =>
            print("  [S]")
            SuccessfulParse(result.get, ast, parsedTokens)
          case _ =>
            print("  [F]")
            FailedParse(ParserError("Unspecified parser error"), unrecognizedTokens)
        }
    }
  }
}

object Server extends ServerApp {
  import MemoParser.{ FailedParse, SuccessfulParse }

  case class ParseRequest(input: String, mode: String)

  sealed trait Response
  case class ErrorResponse(error: String) extends Response

  // TODO factor out probably
  case class Hashes(input: String, output: String, hmac: String)
  object Hashes {
    val key: String = sys.env.get("HMAC_PRIVATE_KEY").get

    def apply(input: String, output: String): Hashes = {
      val inputHash = input.md5.hex
      val outputHash = output.md5.hex
      val hmac = s"${inputHash}.${outputHash}".hmac(key).sha512.hex
      Hashes(inputHash, outputHash, hmac)
    }

    def verify(hashes: Hashes): Boolean = {
      s"${hashes.input}.${hashes.output}".hmac(key).sha512 hash= hashes.hmac
    }
  }

  case class SuccessfulParseResponse(input: String, js: String, tokens: Seq[String], hashes: Hashes, version: String) extends Response
  object SuccessfulParseResponse {
    def apply(input: String, js: String, tokens: Seq[String]): SuccessfulParseResponse = {
      SuccessfulParseResponse(input, js, tokens, Hashes(input, js), Parser.VERSION)
    }
  }

  case class FailedParseResponse(error: String, suggestions: Seq[String], unrecognizedTokens: Seq[String], version: String) extends Response
  object FailedParseResponse {
    def apply(error: ParserError = ParserError("Parse failed"), unrecognizedTokens: Seq[String] = Seq.empty): FailedParseResponse = {
      FailedParseResponse(error.description, error.suggestions.toSeq, unrecognizedTokens, Parser.VERSION)
    }
  }

  case class VersionResponse(version: String, sha: String)
  case class VerifyHashesResponse(input: String, valid: Boolean)

  object InputParamMatcher extends QueryParamDecoderMatcher[String]("input")
  object FormatParamMatcher extends OptionalQueryParamDecoderMatcher[String]("format")
  object ModeParamMatcher extends OptionalQueryParamDecoderMatcher[String]("mode")  // "object" (i.e. robot/structure), "event" (i.e. action), or unspecified
  object FastModeParamMatcher extends OptionalQueryParamDecoderMatcher[Boolean]("fast")

  val host = "0.0.0.0"
  val defaultPort = 8080
  val port: Int = (Option(System.getenv("PORT")) orElse Option(System.getenv("HTTP_PORT"))).map(_.toInt).getOrElse(defaultPort)

  val service: HttpService = HttpService {
    case GET -> Root / "version" =>
      Ok(VersionResponse(Parser.VERSION, sys.env.get("HEROKU_SLUG_COMMIT").getOrElse("local")).asJson, headers())

    case OPTIONS -> Root / "parse" =>
      Ok("", headers())
    case OPTIONS -> Root / "verify-hashes" =>
      Ok("", headers())

    case GET -> Root / "parse" :? InputParamMatcher(input) +& FormatParamMatcher(format) +& ModeParamMatcher(validationMode) +& FastModeParamMatcher(fastMode) =>
      MemoParser(input, validationMode, fastMode.getOrElse(false)) match {
        case SuccessfulParse(parse, ast, parsedTokens) =>
          format match {
            case Some("js") =>
              CodeGenerator.generateJS(ast) match {
                case Success(js: String) => successResponse(input, js, parsedTokens)
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
          val parseResponse: Response = MemoParser(req.input, Option(req.mode), fastErrorAnalysisMode = true) match {
            case SuccessfulParse(_, ast, parsedTokens) =>
              CodeGenerator.generateJS(ast) match {
                case Success(js: String) => SuccessfulParseResponse(req.input, js, parsedTokens)
                case Failure(ex: Throwable) => FailedParseResponse(ParserError(s"Invalid JavaScript produced: ${ex.getMessage}. Contact the developers."))
              }
            case FailedParse(error, unrecognizedTokens) => FailedParseResponse(error, unrecognizedTokens)
          }
          req.input -> parseResponse
        }.asJson
        Ok(responseBody, headers())
      }

    case request @ POST -> Root / "verify-hashes" =>
      implicit val hashesDecoder: Decoder[Hashes] = Decoder.forProduct3("input", "output", "hmac")(Hashes.apply)

      request.as(jsonOf[Seq[Hashes]]).flatMap { hashes: Seq[Hashes] =>
        // scalastyle:off regex
        println(s"Verifying ${hashes.length} hashes ...")
        // scalastyle:on regex
        val responseBody: Json = hashes.map { h => VerifyHashesResponse(h.input, Hashes.verify(h)) }.asJson
        Ok(responseBody, headers())
      }

    case GET -> Root / "lexicon" :? FormatParamMatcher(format) =>
      format match {
        case Some("json") => Ok(Lexicon.asJson, headers())
        case None => Ok(s"I can understand ${Lexicon.listOfTerms.size.toString} terms:\n\n${Lexicon.listOfTerms.mkString("\n")}", headers())
        case _ => BadRequest(ErrorResponse("Invalid format").asJson, headers())
      }
  }

  def successResponse(input: String, js: String, parsedTokens: Seq[String] = Seq()): Task[H4sResponse] = {
    Ok(SuccessfulParseResponse(input, js, parsedTokens).asJson, headers())
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

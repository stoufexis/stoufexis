//> using scala 3.3
//> using toolkit typelevel:0.1.29
//> using dep "io.circe::circe-parser:0.14.10"

import io.circe.parser.*
import io.circe.*
import cats.effect.*
import cats.implicits.given
import fs2.io.file.*
import fs2.*
import org.http4s.client.*
import org.http4s.circe.{given, *}
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.*
import cats.effect.std.Env
import java.time.*

case class Config(base_api_url: String, pins: List[String], source_at: String) derives Codec

case class Repo(
  name             : String, 
  html_url         : String, 
  description      : String, 
  languages_url    : String, 
  stargazers_count : Int, 
  created_at       : ZonedDateTime, 
  pushed_at        : ZonedDateTime
) derives Codec

given EntityDecoder[IO, Repo] = jsonOf

def loadConfig(path: Path): IO[Config] =
  for
    string <- Files[IO].readUtf8(path).compile.string
    cfg    <- IO.fromEither(parse(string).flatMap(_.as[Config]))
  yield cfg

def pin(base: String, repo: String, client: Client[IO]): IO[List[String]] =
  val jsonObError: Exception =
    RuntimeException("Unexpected value for languages: was not an object")

  for
    repo      <- client.expect[Repo](base + repo)
    langs     <- client.expect[Json](repo.languages_url)
    langsList <- IO.fromOption(langs.asObject.map(_.keys.toList))(jsonObError)
  yield List(
    s"* **[${repo.name}](${repo.html_url})**: ${repo.description}",
    s"  * ${langsList.take(5).mkString(", ")} | ${YearMonth.from(repo.created_at)} - ${YearMonth.from(repo.pushed_at)}",
    ""
  )

def generateFile(path: Path, lines: List[String]): IO[Unit] =
  Stream
    .iterable(lines)
    .through(Files[IO].writeUtf8Lines(path))
    .compile
    .drain

def loadLines(path: Path): IO[List[String]] =
  Files[IO].readUtf8Lines(path).compile.toList

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    (Env[IO].get("README_PATH"), Env[IO].get("CONFIG_PATH"), Env[IO].get("PRELUDE_PATH"))
      .flatMapN:
        case (Some(rPathStr), Some(cPathStr), Some(pPathStr)) =>
          EmberClientBuilder.default[IO].build.use: client =>
            val rPath = Path(rPathStr)
            val cPath = Path(cPathStr)
            val pPath = Path(pPathStr)

            for
              cfg     <- loadConfig(cPath)
              prelude <- loadLines(pPath)
              pins    <- cfg.pins.flatTraverse(pin(cfg.base_api_url, _, client))
              _       <- generateFile(rPath, prelude ++ ("" :: pins) :+ s"_[Source Code](${cfg.source_at})_")
            yield ExitCode.Success

        case args =>
          IO.raiseError(RuntimeException("an environment variable is missing"))
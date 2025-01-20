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

case class Config(base_api_url: String, pins: List[String]) derives Codec

case class Repo(name: String, html_url: String, description: String, language: String, stargazers_count: Int) derives Codec

given EntityDecoder[IO, Repo] = jsonOf

def loadConfig(path: Path): IO[Config] =
  for
    string <- Files[IO].readUtf8(path).compile.string
    cfg    <- IO.fromEither(parse(string).flatMap(_.as[Config]))
  yield cfg

def pin(base: String, repo: String, client: Client[IO]): IO[List[String]] =
  client.expect[Repo](base + repo).map: repo =>
    List(
      s"* **[${repo.name}](${repo.html_url})**",
      s"    * ${repo.description}",
      s"    * ${repo.language}",
      s"    * :star: ${repo.stargazers_count}",
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
    Env[IO]
      .get("README_PATH")
      .product(Env[IO].get("CONFIG_PATH"))
      .product(Env[IO].get("PRELUDE_PATH"))
      .flatMap:
        case ((Some(rPathStr), Some(cPathStr)), Some(pPathStr)) =>
          EmberClientBuilder.default[IO].build.use: client =>
            val rPath = Path(rPathStr)
            val cPath = Path(cPathStr)
            val pPath = Path(pPathStr)

            for
              cfg     <- loadConfig(cPath)
              prelude <- loadLines(pPath)
              pins    <- cfg.pins.flatTraverse(pin(cfg.base_api_url, _, client))
              _       <- generateFile(rPath, prelude ++ pins)
            yield ExitCode.Success

        case args =>
          IO.raiseError(RuntimeException("environment variable README_PATH or CONFIG_PATH not defined"))
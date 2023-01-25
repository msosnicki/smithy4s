/*
 *  Copyright 2021-2022 Disney Streaming
 *
 *  Licensed under the Tomorrow Open Source Technology License, Version 1.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *     https://disneystreaming.github.io/TOST-1.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package smithy4s.aws.http4s

import cats.effect.{IO, IOApp, Resource}
import cats.effect.kernel.Deferred
import cats.effect.std.Dispatcher
import cats.effect.syntax.resource._
import cats.implicits._
import com.amazonaws.dynamodb._
import org.http4s.{Request, Response}
import org.http4s.client.Client
import smithy4s.aws._
import software.amazon.awssdk.http.{
  ExecutableHttpRequest,
  HttpExecuteRequest,
  HttpExecuteResponse,
  SdkHttpClient,
  SdkHttpRequest
}
import software.amazon.awssdk.services.dynamodb.DynamoDbClient

import scala.jdk.CollectionConverters._
import smithy4s.aws.kernel.AwsRegion

/**
  * This test builds a request using the AWS Official SDK client for
  * DynamoDB. It uses a HTTP client that always fail but records the request
  * details for comparison. It does the same thing for a Smithy4s generated
  * DynamoDB client.
  * Currently the tow pieces of information are just dumped in stdout.
  */
object SignatureTest extends IOApp.Simple {
  private def buildStsClient(
      deferred: Deferred[IO, SdkRequestData],
      dispatcher: Dispatcher[IO]
  ) =
    Resource.fromAutoCloseable(
      IO.delay(
        DynamoDbClient.builder.httpClient(newClient(deferred, dispatcher)).build
      )
    )

  def run: IO[Unit] = {
    val program = for {
      dispatcher <- Dispatcher.parallel[IO]
      deferred <- Deferred[IO, SdkRequestData].toResource
      awsSdkData <- viaAwsSdk(dispatcher).toResource
      smithy4sData <- viaSmithy4s().toResource
      _ <- compareSignature(awsSdkData, smithy4sData).toResource
    } yield ()
    program.use_
  }

  def compareSignature(
      sdkData: SdkRequestData,
      smithy4sData: SdkRequestData
  ): IO[Unit] = {
    println(s"SDK: ${sdkData.headers.get("Authorization")}")
    println(s"Smithy4s: ${smithy4sData.headers.get("Authorization")}")
    IO.unit
  }

  def viaAwsSdk(dispatcher: Dispatcher[IO]): IO[SdkRequestData] = {
    Deferred[IO, SdkRequestData].flatMap { deferred =>
      buildStsClient(deferred, dispatcher).use(sts =>
        IO.delay(sts.listTables()).attemptNarrow[ShortCircuitException]
      ) *> deferred.get
    }
  }

  def viaSmithy4s(): IO[SdkRequestData] = {
    Deferred[IO, SdkRequestData].flatMap { deferred =>
      val client = Client[IO] { request =>
        val data = SdkRequestData.fromHttp4s(request)
        val run = deferred.complete(data) *>
          request.body.compile.drain *>
          IO.raiseError[Response[IO]](new ShortCircuitException())
        run.toResource
      }
      val simpleClient: SimpleHttpClient[IO] = AwsHttp4sBackend(client)
      val credentialsF =
        AwsCredentialsProvider
          .defaultCredentialsFile[IO]
          .flatMap(path =>
            AwsCredentialsProvider
              .fromDisk[IO](path, AwsCredentialsProvider.getProfileFromEnv)
          )
      val env = AwsEnvironment.make(
        simpleClient,
        IO.pure(AwsRegion.US_EAST_1),
        credentialsF,
        // note: fromEpochMilli would be nice
        IO.realTime.map(_.toSeconds).map(Timestamp(_, 0))
      )
      AwsClient
        .prepare(DynamoDB)
        .leftWiden[Throwable]
        .liftTo[IO]
        .flatMap { interpreter =>
          val ddb = interpreter.buildSimple(env)
          ddb
            .listTables()
            .attemptNarrow[ShortCircuitException]
        }
        .productR(deferred.get)
    }
  }

  def newClient(
      deferred: Deferred[IO, SdkRequestData],
      dispatcher: Dispatcher[IO]
  ): SdkHttpClient = new SdkHttpClient {
    override def prepareRequest(
        request: HttpExecuteRequest
    ): ExecutableHttpRequest = {
      new ExecutableHttpRequest {
        override def call(): HttpExecuteResponse = {
          val data = SdkRequestData.fromAwsSdk(request.httpRequest())
          dispatcher.unsafeRunAndForget(deferred.complete(data))
          throw new ShortCircuitException()
        }
        override def abort(): Unit = ()
      }
    }

    override def close(): Unit = ()
  }
}

class ShortCircuitException extends scala.util.control.NoStackTrace

final case class SdkRequestData(headers: Map[String, List[String]])

object SdkRequestData {
  def fromAwsSdk(request: SdkHttpRequest): SdkRequestData = {
    SdkRequestData(
      request.headers.asScala.map { case (key, list) =>
        (key, list.asScala.toList)
      }.toMap
    )
  }

  def fromHttp4s(request: Request[IO]): SdkRequestData = {
    SdkRequestData(
      request.headers.headers
        .map(rh => (rh.name.toString, List(rh.value)))
        .toMap
    )
  }
}

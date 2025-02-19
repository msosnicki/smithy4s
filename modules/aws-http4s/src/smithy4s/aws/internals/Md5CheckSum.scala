/*
 *  Copyright 2021-2025 Disney Streaming
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

package smithy4s.aws.internals

import cats.effect.Resource
import cats.effect.Sync
import cats.syntax.all._
import fs2.Chunk
import fs2.Pipe
import fs2.Stream
import org.http4s._
import org.http4s.client.Client
import org.typelevel.ci.CIString
import smithy4s.Endpoint
import smithy4s.Service

private[aws] object Md5CheckSum {

  def middleware[F[_]: Sync]: Endpoint.Middleware[Client[F]] =
    new Endpoint.Middleware[Client[F]] {
      def prepare[Alg[_[_, _, _, _, _]]](service: Service[Alg])(
          endpoint: service.Endpoint[_, _, _, _, _]
      ): Client[F] => Client[F] = client =>
        if (endpoint.hints.has(smithy.api.HttpChecksumRequired)) {
          reqWithChecksum(client)
        } else client
    }

  private def reqWithChecksum[F[_]: Sync](client: Client[F]): Client[F] = {
    val md5HeaderPipe: Pipe[F, Byte, String] =
      fs2.hash.md5[F] andThen fs2.text.base64.encode[F]
    Client { request =>
      val withChecksum =
        for {
          body <- request.body.compile.to(Chunk)
          bodyHash <- fs2.Stream
            .emits(body.toList)
            .through(md5HeaderPipe)
            .compile
            .to(Chunk)
          md5Header = Header.Raw(
            CIString("Content-MD5"),
            bodyHash.mkString_("")
          )
          res = request.putHeaders(md5Header).withBodyStream(Stream.chunk(body))
        } yield res

      Resource.eval(withChecksum).flatMap { request =>
        client.run(request)
      }
    }
  }
}

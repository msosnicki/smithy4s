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

package smithy4s.aws

import aws.protocols._
import smithy4s.Hints
import smithy4s.ShapeTag

private[aws] sealed trait AwsProtocol extends Product with Serializable {}

private[aws] object AwsProtocol {
  val supportedProtocols: List[ShapeTag[_]] =
    List(Ec2Query, AwsJson1_0, AwsJson1_1, AwsQuery, RestJson1, RestXml)

  def apply(hints: Hints): Option[AwsProtocol] =
    hints
      .get(Ec2Query)
      .map(AWS_EC2_QUERY.apply)
      .orElse(
        hints
          .get(AwsJson1_0)
          .map(AWS_JSON_1_0.apply)
      )
      .orElse(
        hints
          .get(AwsJson1_1)
          .map(AWS_JSON_1_1.apply)
      )
      .orElse(
        hints
          .get(AwsQuery)
          .map(AWS_QUERY.apply)
      )
      .orElse(
        hints
          .get(RestJson1)
          .map(AWS_REST_JSON_1.apply)
      )
      .orElse(
        hints
          .get(RestXml)
          .map(AWS_REST_XML.apply)
      )

  // See https://smithy.io/2.0/aws/protocols/aws-ec2-query-protocol.html.
  final case class AWS_EC2_QUERY(value: Ec2Query) extends AwsProtocol

  // See https://smithy.io/2.0/aws/protocols/aws-json-1_0-protocol.html,
  // https://smithy.io/2.0/aws/protocols/aws-json-1_1-protocol.html, and
  // https://smithy.io/2.0/aws/protocols/aws-json-1_0-protocol.html#differences-between-awsjson1-0-and-awsjson1-1.
  final case class AWS_JSON_1_0(value: AwsJson1_0) extends AwsProtocol
  final case class AWS_JSON_1_1(value: AwsJson1_1) extends AwsProtocol

  // See https://smithy.io/2.0/aws/protocols/aws-query-protocol.html.
  final case class AWS_QUERY(value: AwsQuery) extends AwsProtocol

  // See https://smithy.io/2.0/aws/protocols/aws-restjson1-protocol.html.
  final case class AWS_REST_JSON_1(value: RestJson1) extends AwsProtocol

  // See https://smithy.io/2.0/aws/protocols/aws-restxml-protocol.html.
  final case class AWS_REST_XML(value: RestXml) extends AwsProtocol

}

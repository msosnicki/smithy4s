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

package smithy4s
package internals

final case class DiscriminatedUnionMember(
    propertyName: String,
    alternativeLabel: String
)

object DiscriminatedUnionMember
    extends ShapeTag.Companion[DiscriminatedUnionMember] {

  val id: ShapeId = ShapeId("smithy4s", "DiscriminatedUnionMember")

  val schema: Schema[DiscriminatedUnionMember] = Schema
    .struct(
      Schema.string
        .required[DiscriminatedUnionMember]("propertyName", _.propertyName),
      Schema.string.required[DiscriminatedUnionMember](
        "alternativeLabel",
        _.alternativeLabel
      )
    )(DiscriminatedUnionMember.apply)
    .withId(id)

}

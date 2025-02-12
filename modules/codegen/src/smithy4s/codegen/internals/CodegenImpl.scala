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

package smithy4s.codegen
package internals

import alloy.openapi._
import smithy4s.codegen.CodegenEntry.FromDisk
import smithy4s.codegen.CodegenEntry.FromMemory
import smithy4s.codegen.transformers._
import software.amazon.smithy.model.Model
import software.amazon.smithy.model.node.Node
import software.amazon.smithy.model.shapes.ModelSerializer
import software.amazon.smithy.model.transform.ModelTransformer
import software.amazon.smithy.openapi.OpenApiConfig
import software.amazon.smithy.model.SourceLocation
import scala.util.matching.Regex

import scala.jdk.CollectionConverters._

private[codegen] object CodegenImpl { self =>

  def generate(args: CodegenArgs): CodegenResult = {
    val smithyBuild = args.smithyBuild
      .map(os.read)
      .map(SmithyBuild.readJson(_))
    val (classloader, model): (ClassLoader, Model) = internals.ModelLoader.load(
      args.specs.map(_.toIO).toSet,
      args.dependencies,
      args.repositories,
      withBuiltinTransformers(args.transformers),
      args.discoverModels,
      args.localJars
    )

    val (scalaFiles, smithyResources) = if (!args.skipScala) {
      val codegenResult =
        CodegenImpl.generate(model, args.allowedNS, args.excludedNS)
      val scalaFiles = codegenResult.map { case (relPath, result) =>
        val fileName = result.name + ".scala"
        val scalaFile = (args.output / relPath / fileName)
        CodegenEntry.FromMemory(scalaFile, result.content)
      }
      val generatedNamespaces = codegenResult.map(_._2.namespace).distinct
      // when args.specs and generatedNamespaces are empty
      // we produce two files that are essentially empty
      val skipResource =
        args.skipResources || (args.specs.isEmpty && generatedNamespaces.isEmpty)
      val resources = if (!skipResource) {
        SmithyResources.produce(
          args.resourceOutput,
          args.specs,
          generatedNamespaces
        )
      } else List.empty[CodegenEntry]
      (scalaFiles, resources)
    } else (List.empty, List.empty)

    val openApiFiles = if (!args.skipOpenapi) {
      val openApiConfig: Unit => OpenApiConfig = _ =>
        smithyBuild
          .flatMap(_.getPlugin[SmithyBuildPlugin.OpenApi])
          .map(_.config)
          .getOrElse(new OpenApiConfig())

      val allowedNS = args.allowedNS.map(_.map(NamespacePattern.fromString))
      val excludedNS = args.excludedNS.map(_.map(NamespacePattern.fromString))

      val allNamespaces =
        model.getShapeIds().asScala.map(_.getNamespace()).toSet
      val isAllowed: String => Boolean = str =>
        allowedNS.map(_.exists(_.matches(str))).getOrElse(true)
      val notExcluded: String => Boolean = str =>
        !excludedNS.getOrElse(Set.empty).exists(_.matches(str))
      val openApiNamespaces = allNamespaces.filter(namespace =>
        isAllowed(namespace) && notExcluded(namespace)
      )
      alloy.openapi
        .convertWithConfig(
          model,
          Some(openApiNamespaces).filter(_ != allNamespaces),
          openApiConfig,
          classloader
        )
        .map { case OpenApiConversionResult(_, serviceId, outputString) =>
          val name = serviceId.getNamespace() + "." + serviceId.getName()
          val openapiFile = (args.resourceOutput / (name + ".json"))
          CodegenEntry.FromMemory(openapiFile, outputString)
        }
    } else List.empty

    val protoFiles = if (!args.skipProto) {
      smithytranslate.proto3.SmithyToProtoCompiler.compile(model).map {
        renderedProto =>
          val protoFile = (args.resourceOutput / renderedProto.path)
          CodegenEntry.FromMemory(protoFile, renderedProto.contents)
      }
    } else List.empty

    CodegenResult(
      sources = scalaFiles,
      resources = openApiFiles ++ protoFiles ++ smithyResources
    )
  }

  def write(result: CodegenResult): Set[os.Path] = {
    def entryToDisk(entry: CodegenEntry): Unit = entry match {
      case FromMemory(path, content) =>
        os.write.over(path, content, createFolders = true)
        ()
      case FromDisk(path, sourceFile) =>
        os.copy.over(
          from = sourceFile,
          to = path,
          replaceExisting = true,
          createFolders = true
        )
    }

    val sourcesPaths = result.sources.map { e =>
      entryToDisk(e)
      e.toPath
    }
    val resourcesPaths = result.resources.map { e =>
      entryToDisk(e)
      e.toPath
    }

    (sourcesPaths ++ resourcesPaths).toSet
  }

  private[internals] def generate(
      model: Model,
      allowedNS: Option[Set[String]],
      excludedNS: Option[Set[String]]
  ): List[(os.RelPath, Renderer.Result)] = {
    val namespaces = model
      .shapes()
      .iterator()
      .asScala
      .map(_.getId().getNamespace())
      .toSet

    val reserved =
      Set(
        "alloy",
        "alloy.common",
        "alloy.proto",
        "smithy4s.api",
        "smithy4s.meta",
        "smithytranslate"
      )

    // Retrieving metadata that indicates what has already been generated by Smithy4s
    // in upstream jars.
    val alreadyGenerated: Set[String] = {
      val records = CodegenRecord.recordsFromModel(model)

      val allGenerated: Seq[String] = records.flatMap { r =>
        r.namespaces
      }
      val allGeneratedSet = allGenerated.toSet

      // If there are any duplicates then the set will be smaller than the list
      if (allGeneratedSet.size != allGenerated.size) {
        // There are duplicates. Find the duplicates and their source, then throw an exception
        val duplicates: Seq[(String, Seq[SourceLocation])] = records
          .flatMap { r =>
            r.namespaces.map(ns => ns -> r.source)
          }
          .groupBy(_._1)
          .collect {
            case (s, l) if l.size > 1 => (s, l)
          } // just the duplicates
          .map { sl => sl._1 -> sl._2.map(_._2) }
          .toSeq
          .sortBy(_._1)

        throw RepeatedNamespaceException(duplicates)
      }

      allGeneratedSet
    }

    val excluded =
      excludedNS.getOrElse(Set.empty).map(NamespacePattern.fromString)
    val allowed = allowedNS.map(_.map(NamespacePattern.fromString))

    val filteredNamespaces = allowed match {
      case Some(allowedNamespaces) =>
        namespaces
          .filter(namespace =>
            allowedNamespaces.exists(_.matches(namespace)) && !excluded.exists(
              _.matches(namespace)
            )
          )
          .filterNot(alreadyGenerated)
      case None =>
        namespaces
          .filterNot(_.startsWith("aws."))
          .filterNot(_.startsWith("smithy."))
          .filterNot(ns => reserved.exists(ns.startsWith))
          .filterNot(namespace => excluded.exists(_.matches(namespace)))
          .filterNot(alreadyGenerated)
    }

    filteredNamespaces.toList
      .map { ns => SmithyToIR(model, ns) }
      .flatMap { cu =>
        val amended = CollisionAvoidance(cu)
        Renderer(amended)
      }
      .map { result =>
        val relPath =
          os.RelPath(result.namespace.split('.').toIndexedSeq, ups = 0)
        (relPath, result)
      }

  }

  def dumpModel(args: DumpModelArgs): String = {
    val (_, model) = ModelLoader.load(
      args.specs.map(_.toIO).toSet,
      args.dependencies,
      args.repositories,
      withBuiltinTransformers(args.transformers),
      discoverModels = false,
      args.localJars
    )
    val flattenedModel =
      ModelTransformer.create().flattenAndRemoveMixins(model)

    Node.prettyPrintJson(
      ModelSerializer.builder().build.serialize(flattenedModel)
    )
  }

  private def withBuiltinTransformers(
      transformers: List[String]
  ): List[String] =
    transformers :+
      AwsConstraintsRemover.name :+
      AwsStandardTypesTransformer.name :+
      OpenEnumTransformer.name :+
      KeepOnlyMarkedShapes.name :+
      ValidatedNewtypesTransformer.name
}

case class RepeatedNamespaceException(
    duplicates: Seq[(String, Seq[SourceLocation])]
) extends IllegalStateException(
      RepeatedNamespaceException.createMessage(duplicates)
    )

object RepeatedNamespaceException {
  private def createMessage(
      duplicates: Seq[(String, Seq[SourceLocation])]
  ): String = {
    val duplicateMessages = duplicates.map { d =>
      s"${d._1} is contained in ${d._2.size} artifacts:\n  ${d._2.mkString("\n  ")}"
    }
    s"""Multiple artifact manifests cannot contain generated code for the same namespace:
       | ${duplicateMessages.mkString("\n")}""".stripMargin
  }
}

private[internals] final case class NamespacePattern private (pattern: String) {
  import NamespacePattern._
  private val regexPattern =
    new Regex(
      pattern
        .split("\\.")
        .map {
          case "*"                      => "[a-z0-9_\\.]*"
          case wildcardSegment(segment) => s"$segment([a-z0-9_\\.])*"
          case validSegment(segment)    => segment
          case _                        => "(?!).*"
        }
        .mkString("^", "\\.", "$")
    )

  def matches(namespace: String): Boolean =
    regexPattern.pattern.matcher(namespace).matches()
}

private[internals] object NamespacePattern {
  val wildcardSegment = "([a-z][a-z0-9_]*)\\*".r
  val validSegment = "([a-z][a-z0-9_]*)".r
  def fromString(str: String): NamespacePattern = new NamespacePattern(str)
}

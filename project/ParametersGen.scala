import java.io.File
import java.nio.charset.StandardCharsets

import scala.jdk.CollectionConverters._

import org.yaml.snakeyaml.LoaderOptions
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.SafeConstructor

import sbt.io.IO
import sbt.io.syntax._

/** ParametersGen generates the Scala code of recheck's parameters from `parameters.schema.yaml`. */
object ParametersGen {

  /** The schema path relative to the repository root. */
  val SchemaPath: String = "modules/recheck-common/parameters.schema.yaml"

  /** Param is a parameter of the schema. */
  final case class Param(
      name: String,
      title: String,
      description: String,
      default: Any,
      values: Seq[String],
      scalaType: String,
      scalaValues: Map[String, String],
      unit: Option[String],
      cli: Option[String],
      cliMetavar: Option[String],
      dialects: Seq[String],
      encodings: Seq[(String, Seq[String])]
  ) {
    def isDuration: Boolean = scalaType == "Duration"
    def isLogger: Boolean = scalaType == "Option[Context.Logger]"
    def isEnum: Boolean = values.nonEmpty
    def isOption: Boolean = scalaType.startsWith("Option[")
    def capitalName: String = name.capitalize
    def defaultName: String = s"Parameters.Default$capitalName"

    /** The Scala expression of the default value. */
    def scalaDefault: String = default match {
      case null if isDuration                                 => "Duration.Inf"
      case null                                               => "None"
      case n: Number if isDuration && n.longValue % 1000 == 0 => s"Duration(${n.longValue / 1000}, SECONDS)"
      case n: Number if isDuration                            => s"Duration(${n.longValue}, MILLISECONDS)"
      case s: String if isEnum                                => scalaValues(s)
      case s: String                                          => s"Some(${literal(s)})"
      case b: java.lang.Boolean if isOption                   => s"Some($b)"
      case v                                                  => v.toString
    }

    /** The first paragraph of the description on one line. */
    def summary: String = description.split("\n\n").head.replace('\n', ' ')

    /** The error message's name of the value. */
    def label: String = title.toLowerCase
  }

  /** The Scala imports of the types in the schema. */
  private val typeImports: Map[String, String] = Map(
    "AccelerationMode" -> "codes.quine.labs.recheck.common.AccelerationMode",
    "Checker" -> "codes.quine.labs.recheck.common.Checker",
    "Seeder" -> "codes.quine.labs.recheck.common.Seeder",
    "Dialect" -> "codes.quine.labs.resyntax.ast.Dialect",
    "PerlSource" -> "codes.quine.labs.resyntax.ast.PerlSource"
  )

  /** Reads the parameters of the schema. */
  def read(schema: File): Seq[Param] = {
    val yaml = new Yaml(new SafeConstructor(new LoaderOptions))
    val root = yaml.load[java.util.Map[String, Any]](IO.read(schema, StandardCharsets.UTF_8))
    val properties = root.get("properties").asInstanceOf[java.util.Map[String, java.util.Map[String, Any]]]
    properties.asScala.toSeq.map { case (name, p) =>
      def string(key: String): Option[String] = Option(p.get(key)).map(_.toString)
      val values = Option(p.get("enum")).map(_.asInstanceOf[java.util.List[String]].asScala.toSeq).getOrElse(Seq.empty)
      val scalaValues = Option(p.get("x-scala-values"))
        .map(_.asInstanceOf[java.util.Map[String, String]].asScala.toMap)
        .getOrElse(Map.empty[String, String])
      require(values.forall(scalaValues.contains), s"x-scala-values of $name must map every enum value")
      val dialects = Option(p.get("x-dialects")).map(_.asInstanceOf[java.util.List[String]].asScala.toSeq)
      val encodings = Option(p.get("x-encodings"))
        .map(_.asInstanceOf[java.util.Map[String, java.util.List[String]]].asScala.toSeq.map { case (d, es) =>
          (d, es.asScala.toSeq)
        })
      Param(
        name = name,
        title = string("title").getOrElse(sys.error(s"$name has no title")),
        description = string("description").getOrElse(sys.error(s"$name has no description")),
        default = p.get("default"),
        values = values,
        scalaType = string("x-scala-type").getOrElse(sys.error(s"$name has no x-scala-type")),
        scalaValues = scalaValues,
        unit = string("x-unit"),
        cli = string("x-cli"),
        cliMetavar = string("x-cli-metavar"),
        dialects = dialects.getOrElse(Seq.empty),
        encodings = encodings.getOrElse(Seq.empty)
      )
    }
  }

  /** Target is a generated Scala source. */
  sealed abstract class Target(val fileName: String, val pkg: Seq[String])

  object Target {
    case object Parameters extends Target("Parameters.scala", Seq("codes.quine.labs.recheck.common"))
    case object Codec extends Target("ParametersCodec.scala", Seq("codes.quine.labs.recheck", "codec"))
    case object Cli extends Target("ParametersOpts.scala", Seq("codes.quine.labs.recheck.cli"))
    case object SchemaTest extends Target("ParametersSchema.scala", Seq("codes.quine.labs.recheck.common"))
  }

  /** Generates the target in the directory, and returns the generated file. */
  def generate(target: Target, root: File, dir: File): File = {
    val params = read(root / SchemaPath)
    val source = target match {
      case Target.Parameters => parameters(params)
      case Target.Codec      => codec(params)
      case Target.Cli        => cli(params)
      case Target.SchemaTest => schemaTest(params)
    }
    val file = target.pkg.flatMap(_.split('.')).foldLeft(dir)(_ / _) / target.fileName
    if (!file.exists() || IO.read(file, StandardCharsets.UTF_8) != source)
      IO.write(file, source, StandardCharsets.UTF_8)
    file
  }

  private val header: String =
    s"// This file is generated from `$SchemaPath` by `project/ParametersGen.scala`. Do not edit it."

  /** Returns a Scala string literal. */
  def literal(s: String): String =
    "\"" + s.flatMap {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case c    => c.toString
    } + "\""

  private def packages(target: Target): String = target.pkg.map(p => s"package $p").mkString("\n")

  private def imports(names: Seq[String]): String = names.distinct.sorted.map(n => s"import $n").mkString("\n")

  private def typeImportsOf(params: Seq[Param], exclude: String): Seq[String] =
    params.flatMap(p => typeImports.get(p.scalaType)).filterNot(_.startsWith(exclude))

  /** Returns the Scaladoc lines of the description. */
  private def docLines(description: String, indent: String): String =
    description.linesIterator.map(l => if (l.isEmpty) s"$indent*" else s"$indent* $l").mkString("\n")

  /** Generates `Parameters.scala`. */
  def parameters(params: Seq[Param]): String = {
    val docs = params.map { p =>
      s"""|  * @param ${p.name}
          |  *
          |${docLines(p.description, "  ")}
          |  *
          |  * (default: `${p.scalaDefault}`)""".stripMargin
    }
    val fields = params.map(p => s"    ${p.name}: ${p.scalaType} = ${p.defaultName}")
    val defaults = params.map { p =>
      s"""|  /** The default value of [[Parameters.${p.name}]]. */
          |  val Default${p.capitalName}: ${p.scalaType} = ${p.scalaDefault}""".stripMargin
    }
    s"""|$header
        |${packages(Target.Parameters)}
        |
        |import scala.concurrent.duration.*
        |
        |${imports(typeImportsOf(params, "codes.quine.labs.recheck.common."))}
        |
        |/** Parameters is an immutable representation of parameters.
        |  *
        |${docs.mkString("\n  *\n")}
        |  */
        |final case class Parameters(
        |${fields.mkString(",\n")}
        |)
        |
        |object Parameters:
        |  // $$COVERAGE-OFF$$
        |
        |${defaults.mkString("\n\n")}
        |
        |  // $$COVERAGE-ON$$
        |""".stripMargin
  }

  /** Generates `ParametersCodec.scala`, the decoders of `Parameters` and its enums. */
  def codec(params: Seq[Param]): String = {
    val fields = params.map { p =>
      s"      ${p.name} <- getOrElse[${p.scalaType}](${literal(p.name)})(${p.defaultName})"
    }
    val args = params.map(p => s"      ${p.name} = ${p.name}")
    val enums = params.filter(_.isEnum).map { p =>
      val cases = p.values.map(v => s"    case ${literal(v)} => Right(${p.scalaValues(v)})")
      s"""|/** A `Decoder` for `${p.scalaType}`. */
          |given decode${p.scalaType}: Decoder[${p.scalaType}] =
          |  Decoder[String].emap:
          |${cases.mkString("\n")}
          |    case s => Left(s"Unknown ${p.label}: $$s")""".stripMargin
    }
    val names = Seq(
      "scala.concurrent.duration.Duration",
      "io.circe.Decoder",
      "io.circe.DecodingFailure.Reason.MissingField",
      "io.circe.HCursor",
      "codes.quine.labs.recheck.common.Context",
      "codes.quine.labs.recheck.common.Parameters"
    ) ++ typeImportsOf(params, "_")
    s"""|$header
        |${packages(Target.Codec)}
        |
        |${imports(names)}
        |
        |/** A `Decoder` for `Parameters`.
        |  *
        |  * A missing key is the default value, and a `null` value is decoded as the field's type does.
        |  */
        |given decodeParameters(using decodeLogger: Decoder[Context.Logger]): Decoder[Parameters] with
        |
        |  def apply(c: HCursor): Decoder.Result[Parameters] =
        |    def getOrElse[A: Decoder](key: String)(fallback: A): Decoder.Result[A] =
        |      c.get[A](key) match
        |        case Left(failure) if failure.reason == MissingField => Right(fallback)
        |        case result                                          => result
        |
        |    for
        |${fields.mkString("\n")}
        |    yield Parameters(
        |${args.mkString(",\n")}
        |    )
        |
        |${enums.mkString("\n\n")}
        |""".stripMargin
  }

  /** Generates `ParametersOpts.scala`, the CLI options of `Parameters`. */
  def cli(params: Seq[Param]): String = {
    val options = params.filter(p => p.cli.isDefined || p.isLogger).map { p =>
      val update = s"map(v => (p: Parameters) => p.copy(${p.name} = v))"
      if (p.isLogger) s"      logger.$update"
      else {
        val tpe = if (p.isOption) p.scalaType.stripPrefix("Option[").stripSuffix("]") else p.scalaType
        val withDefault = if (p.isOption && p.default == null) "orNone" else s"withDefault(${p.defaultName})"
        s"""|      Opts
            |        .option[$tpe](long = ${literal(p.cli.get)}, help = ${literal(p.summary)})
            |        .$withDefault
            |        .$update""".stripMargin
      }
    }
    val arguments = params.filter(_.isEnum).map { p =>
      val cases = p.values.map(v => s"      case ${literal(v)} => Validated.validNel(${p.scalaValues(v)})")
      val metavar = p.cliMetavar.getOrElse(p.label.replace(' ', '-'))
      s"""|  /** `Argument` instance for `${p.scalaType}`. */
          |  given ${p.scalaType.head.toLower}${p.scalaType.tail}Argument: Argument[${p.scalaType}] with
          |    def read(string: String): ValidatedNel[String, ${p.scalaType}] = string match
          |${cases.mkString("\n")}
          |      case s => Validated.invalidNel(s"unknown ${p.label}: $$s")
          |
          |    def defaultMetavar: String = ${literal(metavar)}""".stripMargin
    }
    val names = Seq(
      "cats.data.Validated",
      "cats.data.ValidatedNel",
      "cats.syntax.apply.*",
      "com.monovore.decline.Argument",
      "com.monovore.decline.Opts",
      "codes.quine.labs.recheck.cli.arguments.given",
      "codes.quine.labs.recheck.common.Context",
      "codes.quine.labs.recheck.common.Parameters",
      "scala.concurrent.duration.Duration"
    ) ++ typeImportsOf(params, "_")
    s"""|$header
        |${packages(Target.Cli)}
        |
        |${imports(names)}
        |
        |/** ParametersOpts provides the CLI options of `Parameters`. */
        |object ParametersOpts:
        |
        |  /** Returns the options of `Parameters` with the given option of the logger. */
        |  def apply(logger: Opts[Option[Context.Logger]]): Opts[Parameters] =
        |    val updates = List[Opts[Parameters => Parameters]](
        |${options.mkString(",\n")}
        |    )
        |    updates.foldLeft(Opts(Parameters()))((params, update) => (params, update).mapN((p, f) => f(p)))
        |
        |/** ParameterArguments provides decline's `Argument` instances of the enums of `Parameters`. */
        |object ParameterArguments:
        |
        |${arguments.mkString("\n\n")}
        |""".stripMargin
  }

  /** Generates `ParametersSchema.scala` for the tests, the dialect annotations of the schema. */
  def schemaTest(params: Seq[Param]): String = {
    def seq(ss: Seq[String]): String = ss.map(literal).mkString("Seq(", ", ", ")")
    val dialect = params.find(_.name == "dialect").getOrElse(sys.error("no dialect parameter"))
    val dialectValues = dialect.values.map(v => s"    ${literal(v)} -> ${dialect.scalaValues(v)}")
    val dialects = params.filter(_.dialects.nonEmpty).map(p => s"    ${literal(p.name)} -> ${seq(p.dialects)}")
    val encodings = params.filter(_.encodings.nonEmpty).map { p =>
      val entries = p.encodings.map { case (d, es) => s"${literal(d)} -> ${seq(es)}" }
      s"    ${literal(p.name)} -> Map(${entries.mkString(", ")})"
    }
    s"""|$header
        |${packages(Target.SchemaTest)}
        |
        |import codes.quine.labs.resyntax.ast.Dialect
        |
        |/** ParametersSchema is the dialect annotations of the schema, to test them against the dialects. */
        |object ParametersSchema:
        |
        |  /** The dialect of each value of `dialect`. */
        |  val dialectValues: Map[String, Dialect] = Map(
        |${dialectValues.mkString(",\n")}
        |  )
        |
        |  /** `x-dialects` of the parameters having it. */
        |  val dialects: Map[String, Seq[String]] = Map(
        |${dialects.mkString(",\n")}
        |  )
        |
        |  /** `x-encodings` of the parameters having it. */
        |  val encodings: Map[String, Map[String, Seq[String]]] = Map(
        |${encodings.mkString(",\n")}
        |  )
        |""".stripMargin
  }

  /** Returns the `check` helper of the console, which takes every parameter as a named argument. */
  def consoleCheck(root: File): String = {
    val params = read(root / SchemaPath)
    val args = params.map(p => s"    ${p.name}: ${p.scalaType} = ${p.defaultName}")
    s"""|
        |def check(
        |    source: String,
        |    flags: String,
        |${args.mkString(",\n")}
        |): Diagnostics =
        |  val params = Parameters(${params.map(p => s"${p.name} = ${p.name}").mkString(", ")})
        |  time("check")(ReDoS.check(source, flags, params))
        |""".stripMargin
  }
}

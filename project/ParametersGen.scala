import java.io.File
import java.nio.charset.StandardCharsets

import scala.jdk.CollectionConverters.*

import org.yaml.snakeyaml.LoaderOptions
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.SafeConstructor

import sbt.io.IO
import sbt.io.syntax.*

/** ParametersGen generates the Scala code of recheck's parameters from `parameters.schema.yaml`. */
object ParametersGen:

  /** The schema path relative to the repository root. */
  val SchemaPath: String = "schema/parameters.schema.yaml"

  final case class Param(
      name: String,
      title: String,
      description: String,
      default: Any,
      values: Seq[String],
      scalaType: String,
      scalaValues: Map[String, String],
      cli: Option[String],
      cliMetavar: Option[String],
      scalaDefaultValue: Option[String],
      variants: Seq[Variant]
  ):

    def isDuration: Boolean = scalaType == "Duration"

    def isLogger: Boolean = scalaType == "Option[Context.Logger]"

    def isEnum: Boolean = values.nonEmpty

    def isOption: Boolean = scalaType.startsWith("Option[")

    def capitalName: String = name.capitalize

    def defaultName: String = s"Parameters.Default$capitalName"

    def scalaDefault: String = scalaDefaultValue.getOrElse:
      default match
        case null                                               => "None"
        case n: Number if isDuration && n.longValue % 1000 == 0 => s"Duration(${n.longValue / 1000}, SECONDS)"
        case n: Number if isDuration                            => s"Duration(${n.longValue}, MILLISECONDS)"
        case s: String if isEnum                                => scalaValues(s)
        case v                                                  => v.toString

    /** The first paragraph of the description on one line. */
    def summary: String = description.split("\n\n").head.replace('\n', ' ')

    /** The error message's name of the value. */
    def label: String = title.toLowerCase

  /** Variant is an object of `oneOf` of a parameter, which has `name` as `const` and some settings. */
  final case class Variant(name: String, scalaType: String, settings: Seq[Setting])

  /** Setting is a setting of a variant, which the CLI takes by an option. */
  final case class Setting(key: String, default: Any, scalaType: String, cliHelp: String):

    def isBoolean: Boolean = scalaType == "Boolean"

    /** The long option name, which is `no-…` for a boolean setting whose default is `true`. */
    def cliOption(variant: Variant): String =
      val name = s"${variant.name}-${key.flatMap(c => if c.isUpper then s"-${c.toLower}" else c.toString)}"
      if isBoolean && default == true then s"no-$name" else name

    /** The name of the local value of the option. */
    def valueName(variant: Variant): String = s"${variant.name}${key.capitalize}"

  private val typeImports: Map[String, String] = Map(
    "AccelerationMode" -> "codes.quine.labs.recheck.common.AccelerationMode",
    "Checker" -> "codes.quine.labs.recheck.common.Checker",
    "Seeder" -> "codes.quine.labs.recheck.common.Seeder",
    "Dialect" -> "codes.quine.labs.resyntax.dialect.Dialect",
    "PerlSource" -> "codes.quine.labs.resyntax.dialect.PerlSource"
  )

  private val scalaKeywords: Set[String] = Set(
    "abstract",
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "enum",
    "export",
    "extends",
    "false",
    "final",
    "finally",
    "for",
    "given",
    "if",
    "implicit",
    "import",
    "lazy",
    "match",
    "new",
    "null",
    "object",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "sealed",
    "super",
    "then",
    "this",
    "throw",
    "trait",
    "true",
    "try",
    "type",
    "val",
    "var",
    "while",
    "with",
    "yield"
  )

  /** Returns the identifier, which is quoted by backticks if it is a keyword. */
  private def identifier(name: String): String = if scalaKeywords(name) then s"`$name`" else name

  private def readVariant(v: java.util.Map[String, Any]): Option[Variant] =
    Option(v.get("x-scala-type")).map: scalaType =>
      val properties = v.get("properties").asInstanceOf[java.util.Map[String, java.util.Map[String, Any]]].asScala
      val name = properties("name").get("const").toString
      val settings = properties.toSeq
        .filter(_._1 != "name")
        .map:
          case (key, setting) =>
            def string(field: String): String =
              Option(setting.get(field)).map(_.toString).getOrElse(sys.error(s"$key of $name has no $field"))

            Setting(key, setting.get("default"), string("x-scala-type"), string("x-cli-help"))
      Variant(name, scalaType.toString, settings)

  def read(schema: File): Seq[Param] =
    val yaml = new Yaml(new SafeConstructor(new LoaderOptions))
    val root = yaml.load[java.util.Map[String, Any]](IO.read(schema, StandardCharsets.UTF_8))
    val properties = root.get("properties").asInstanceOf[java.util.Map[String, java.util.Map[String, Any]]]
    properties.asScala.toSeq.map:
      case (name, p) =>
        def string(key: String): Option[String] = Option(p.get(key)).map(_.toString)

        val values =
          Option(p.get("enum")).map(_.asInstanceOf[java.util.List[String]].asScala.toSeq).getOrElse(Seq.empty)
        val scalaValues = Option(p.get("x-scala-values"))
          .map(_.asInstanceOf[java.util.Map[String, String]].asScala.toMap)
          .getOrElse(Map.empty[String, String])
        require(values.forall(scalaValues.contains), s"x-scala-values of $name must map every enum value")
        Param(
          name = name,
          title = string("title").getOrElse(sys.error(s"$name has no title")),
          description = string("description").getOrElse(sys.error(s"$name has no description")),
          default = p.get("default"),
          values = values,
          scalaType = string("x-scala-type").getOrElse(sys.error(s"$name has no x-scala-type")),
          scalaValues = scalaValues,
          cli = string("x-cli"),
          cliMetavar = string("x-cli-metavar"),
          scalaDefaultValue = string("x-scala-default"),
          variants = Option(p.get("oneOf")).toSeq
            .flatMap(_.asInstanceOf[java.util.List[java.util.Map[String, Any]]].asScala)
            .flatMap(readVariant)
        )

  /** Target is a generated Scala source. */
  sealed abstract class Target(val fileName: String, val pkg: Seq[String])

  object Target:

    case object Parameters extends Target("Parameters.scala", Seq("codes.quine.labs.recheck.common"))
    case object Codec extends Target("ParametersCodec.scala", Seq("codes.quine.labs.recheck", "codec"))
    case object CLI extends Target("ParametersOption.scala", Seq("codes.quine.labs.recheck.cli"))

  def generate(target: Target, root: File, dir: File): File =
    val params = read(root / SchemaPath)
    val source = target match
      case Target.Parameters => parameters(params)
      case Target.Codec      => codec(params)
      case Target.CLI        => cli(params)
    val file = target.pkg.flatMap(_.split('.')).foldLeft(dir)(_ / _) / target.fileName
    if !file.exists() || IO.read(file, StandardCharsets.UTF_8) != source then
      IO.write(file, source, StandardCharsets.UTF_8)
    file

  private val header: String =
    s"// This file is generated from `$SchemaPath` by `project/ParametersGen.scala`. Do not edit it."

  def literal(s: String): String =
    "\"" + s.flatMap:
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case c    => c.toString
    + "\""

  private def packages(target: Target): String = target.pkg.map(p => s"package $p").mkString("\n")

  private def imports(names: Seq[String]): String = names.distinct.sorted.map(n => s"import $n").mkString("\n")

  private def typeImportsOf(params: Seq[Param]): Seq[String] = params.flatMap(p => typeImports.get(p.scalaType))

  private def settingImportsOf(params: Seq[Param]): Seq[String] =
    for
      p <- params
      variant <- p.variants
      scalaType <- variant.scalaType +: variant.settings.map(_.scalaType)
      name <- typeImports.get(scalaType.takeWhile(_ != '.'))
    yield name

  private def docLines(description: String, indent: String): String =
    description.linesIterator.map(l => if l.isEmpty then s"$indent*" else s"$indent* $l").mkString("\n")

  /** Generates `Parameters.scala`. */
  def parameters(params: Seq[Param]): String =
    val docs = params.map: p =>
      // The descriptions start with "The ...", which follows "`name` is".
      val description = s"`${p.name}` is ${p.description.head.toLower}${p.description.tail}"
      s"""|${docLines(description, "  ")}
          |  *
          |  * The default is `${p.scalaDefault}`.""".stripMargin
    val fields = params.map(p => s"    ${p.name}: ${p.scalaType} = ${p.defaultName}")
    val defaults = params.map: p =>
      s"""|  /** The default value of [[Parameters.${p.name}]]. */
          |  val Default${p.capitalName}: ${p.scalaType} = ${p.scalaDefault}""".stripMargin
    s"""|$header
        |${packages(Target.Parameters)}
        |
        |import scala.concurrent.duration.*
        |
        |${imports(typeImportsOf(params).filterNot(_.startsWith("codes.quine.labs.recheck.common.")))}
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
        |
        |${defaults.mkString("\n\n")}
        |""".stripMargin

  /** Generates `ParametersCodec.scala`, the decoders of `Parameters` and its enums. */
  def codec(params: Seq[Param]): String =
    val fields = params.map: p =>
      s"      ${p.name} <- getOrElse[${p.scalaType}](${literal(p.name)})(${p.defaultName})"
    val args = params.map(p => s"      ${p.name} = ${p.name}")
    val enums = params
      .filter(_.isEnum)
      .map: p =>
        val cases = p.values.map(v => s"    case ${literal(v)} => Right(${p.scalaValues(v)})")
        s"""|/** A `Decoder` for `${p.scalaType}`. */
          |given decode${p.scalaType}: Decoder[${p.scalaType}] =
          |  Decoder[String].emap:
          |${cases.mkString("\n")}
          |    case s => Left(s"Unknown ${p.label}: $$s")""".stripMargin
    val names = Seq(
      "scala.concurrent.duration.Duration",
      "io.circe.Decoder",
      "io.circe.DecodingFailure.Reason.MissingField",
      "io.circe.HCursor",
      "codes.quine.labs.recheck.common.Context",
      "codes.quine.labs.recheck.common.Parameters"
    ) ++ typeImportsOf(params)
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

  /** Generates `ParametersOption.scala`, the CLI options of `Parameters`. */
  def cli(params: Seq[Param]): String =
    val options = params
      .filter(p => p.cli.isDefined || p.isLogger)
      .map: p =>
        val update = s"map(v => (p: Parameters) => p.copy(${p.name} = v))"
        if p.isLogger then s"      logger.$update"
        else
          val tpe = if p.isOption then p.scalaType.stripPrefix("Option[").stripSuffix("]") else p.scalaType
          val withDefault = if p.isOption && p.default == null then "orNone" else s"withDefault(${p.defaultName})"
          s"""|      Opts
            |        .option[$tpe](long = ${literal(p.cli.get)}, help = ${literal(p.summary)})
            |        .$withDefault
            |        .$update""".stripMargin
    val variantParams = params.filter(_.variants.nonEmpty)
    val result = variantParams match
      case Seq()  => "    params"
      case Seq(p) =>
        s"""|    (params, ${p.name}Settings).tupled.mapValidated: (params, settings) =>
            |      settings(params.${p.name}).map(v => params.copy(${p.name} = v))""".stripMargin
      case _ => sys.error("only one parameter may have variants")
    val arguments = params
      .filter(_.isEnum)
      .map: p =>
        val cases = p.values.map(v => s"      case ${literal(v)} => Validated.validNel(${p.scalaValues(v)})")
        val metavar = p.cliMetavar.getOrElse(p.label.replace(' ', '-'))
        s"""|  /** `Argument` instance for `${p.scalaType}`. */
          |  given ${p.scalaType.head.toLower}${p.scalaType.tail}Argument: Argument[${p.scalaType}] with
          |    def read(string: String): ValidatedNel[String, ${p.scalaType}] = string match
          |${cases.mkString("\n")}
          |      case s => Validated.invalidNel(s"unknown ${p.label}: $$s")
          |
          |    def defaultMetavar: String = ${literal(metavar)}""".stripMargin
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
    ) ++ typeImportsOf(params) ++ settingImportsOf(params)
    s"""|$header
        |${packages(Target.CLI)}
        |
        |${imports(names)}
        |
        |/** ParametersOption provides the CLI options of `Parameters`. */
        |object ParametersOption:
        |
        |  /** Returns the options of `Parameters` with the given option of the logger. */
        |  def apply(logger: Opts[Option[Context.Logger]]): Opts[Parameters] =
        |    val updates = List[Opts[Parameters => Parameters]](
        |${options.mkString(",\n")}
        |    )
        |    val params = updates.foldLeft(Opts(Parameters()))((params, update) => (params, update).mapN((p, f) => f(p)))
        |$result
        |
        |${variantParams.map(variantSettings).mkString("\n\n")}
        |
        |  /** Returns the value of the option, or the default if the option is not given. */
        |  private def readSetting[A](option: String, value: Option[String], default: A)(
        |      of: String => Option[A]
        |  ): ValidatedNel[String, A] =
        |    value.fold(Validated.validNel(default)): name =>
        |      Validated.fromOption(of(name), s"unknown value of --$$option: $$name").toValidatedNel
        |
        |/** ParametersArgument provides decline's `Argument` instances of the enums of `Parameters`. */
        |object ParametersArgument:
        |
        |${arguments.mkString("\n\n")}
        |""".stripMargin

  /** Returns the options of the settings of the variants of the parameter, which set them on the variant that the
    * parameter's option gives.
    *
    * An option of another variant is an error.
    */
  private def variantSettings(p: Param): String =
    val pairs = for variant <- p.variants; setting <- variant.settings yield (variant, setting)
    require(pairs.nonEmpty, s"${p.name} has no settings")
    val optionValues = pairs.map: (variant, setting) =>
      val option = literal(setting.cliOption(variant))
      val help = literal(s"${setting.cliHelp.stripSuffix(".")} (${variant.name}).")
      val opts =
        if setting.isBoolean then s"Opts.flag(long = $option, help = $help).map(_ => ${setting.default != true})"
        else s"Opts.option[String](long = $option, help = $help, metavar = ${literal(setting.key)})"
      s"    val ${setting.valueName(variant)} = $opts.orNone"
    val valueNames = pairs.map((variant, setting) => setting.valueName(variant))
    val combined =
      if valueNames.size == 1 then s"${valueNames.head}.map: ${valueNames.head} =>"
      else s"(${valueNames.mkString(", ")}).mapN: (${valueNames.mkString(", ")}) =>"
    val misplaced = pairs.map: (variant, setting) =>
      s"          (${literal(setting.cliOption(variant))}, ${setting.valueName(variant)}.isDefined, ${literal(variant.name)})"
    val cases = p.variants.map: variant =>
      val bound = identifier(variant.name)
      val values = variant.settings.map: setting =>
        val name = setting.valueName(variant)
        val default = s"$bound.${identifier(setting.key)}"
        if setting.isBoolean then s"Validated.validNel[String, Boolean]($name.getOrElse($default))"
        else s"readSetting(${literal(setting.cliOption(variant))}, $name, $default)(${setting.scalaType}.of)"
      val keys = variant.settings.map(setting => identifier(setting.key))
      val copy = s"$bound.copy(${keys.map(k => s"$k = $k").mkString(", ")})"
      val update =
        if values.size == 1 then s"${values.head}.map(${keys.head} => $copy)"
        else s"(${values.mkString(", ")}).mapN((${keys.mkString(", ")}) => $copy)"
      s"""|              case $bound: ${variant.scalaType} =>
          |                $update""".stripMargin
    s"""|  /** Returns the options of the settings of `${p.name}`, which set them on the value of `--${p.cli.get}`. */
        |  private def ${p.name}Settings: Opts[${p.scalaType} => ValidatedNel[String, ${p.scalaType}]] =
        |${optionValues.mkString("\n")}
        |    $combined
        |      (${p.name}: ${p.scalaType}) =>
        |        val misplaced = Seq(
        |${misplaced.mkString(",\n")}
        |        ).collectFirst:
        |          case (option, true, name) if name != ${p.name}.name => option
        |        misplaced match
        |          case Some(option) => Validated.invalidNel(s"--$$option is not an option of $${${p.name}.name}")
        |          case None         =>
        |            ${p.name} match
        |${cases.mkString("\n")}
        |              case _ => Validated.validNel(${p.name})""".stripMargin

  /** Returns the `check` helper of the console, which takes every parameter as a named argument. */
  def consoleCheck(root: File): String =
    val params = read(root / SchemaPath)
    // `dialect` follows `flags`, so that it is given without its name.
    val (dialect, others) = params.partition(_.name == "dialect")
    val args = (dialect ++ others).map: p =>
      val default = if p.isLogger then "Some(logger)" else p.defaultName
      s"    ${p.name}: ${p.scalaType} = $default"

    s"""|
        |def check(
        |    source: String,
        |    flags: String,
        |${args.mkString(",\n")}
        |): Diagnostics =
        |  val params = Parameters(${params.map(p => s"${p.name} = ${p.name}").mkString(", ")})
        |  time("check")(ReDoS.check(source, flags, params))
        |""".stripMargin

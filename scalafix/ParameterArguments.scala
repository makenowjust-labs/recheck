// Scalafix compiles this file with Scala 2.13.
package fix

import java.nio.file.Files
import java.nio.file.Paths
import scala.jdk.CollectionConverters._
import scala.meta._

import scalafix.lint.Diagnostic
import scalafix.v1._

/** ParameterArguments checks the arguments carrying the parameters of `schema/parameters.schema.yaml` in the main
  * sources of the modules of recheck.
  *
  * In an argument list, those arguments follow a comment `Parameters:`, and they are the last ones. They have the
  * names of the parameters in the order of the schema, without default values. `limit`, `timeout` and `maxSize` in
  * this order precede them, which carry a limit, a timeout and a string size chosen by the caller, so no parameter of
  * the same kind is with them.
  */
class ParameterArguments extends SyntacticRule("ParameterArguments") {

  private val Marker = "Parameters:"

  /** The arguments carrying a parameter chosen by the caller, with the suffix of the names of such parameters. */
  private val generics = Seq("limit" -> "Limit", "timeout" -> "Timeout", "maxSize" -> "StringSize")

  /** The names of the parameters that an argument before the comment may have for another value. */
  private val ambiguous = Set("checker", "dialect", "logger", "timeout")

  private lazy val names: Seq[String] = {
    val name = """  ([A-Za-z]\w*):\s*""".r
    Files.readAllLines(Paths.get("schema/parameters.schema.yaml")).asScala.toSeq.collect { case name(n) => n }
  }

  /** Returns the position of the argument in the order, or `None` if it has no name of a parameter. */
  private def order(name: String): Option[Int] =
    generics.indexWhere(_._1 == name) match {
      case -1 => Some(names.indexOf(name)).filter(_ >= 0).map(_ + generics.size)
      case i  => Some(i)
    }

  private def lint(param: Term.Param, message: String): Patch =
    Patch.lint(Diagnostic("", s"the argument ${param.name.value} $message", param.pos))

  override def fix(implicit doc: SyntacticDocument): Patch = {
    val path = doc.input match {
      case Input.VirtualFile(path, _) => path
      case Input.File(path, _)        => path.toString
      case _                          => ""
    }
    if (!path.contains("/modules/recheck") || !path.contains("/src/main/")) Patch.empty
    else {
      val markers = doc.tokens.collect { case c: Token.Comment if c.value.trim == Marker => c.pos.start }
      val patches = doc.tree.collect { case clause: Term.ParamClause =>
        val marker = markers.find(m => clause.pos.start <= m && m < clause.pos.end)
        val (before, after) = clause.values.partition(p => marker.forall(p.pos.start < _))
        val unmarked = before
          .filter(p => names.contains(p.name.value) && !ambiguous.contains(p.name.value))
          .map(lint(_, s"of a parameter is not after `$Marker`"))
        val defaults = after.filter(_.default.isDefined).map(lint(_, "of a parameter has a default value"))
        val unknown = after.filter(p => order(p.name.value).isEmpty).map(lint(_, "has no name of a parameter"))
        val ordered = after.filter(p => order(p.name.value).isDefined)
        val unordered = ordered
          .zip(ordered.drop(1))
          .collect { case (a, b) if order(a.name.value).get > order(b.name.value).get => b }
          .map(lint(_, "is out of the order of the parameters"))
        val mixed = for {
          (generic, suffix) <- generics
          if after.exists(_.name.value == generic)
          p <- after
          if p.name.value != generic && p.name.value.endsWith(suffix)
        } yield lint(p, s"is with $generic")
        unmarked ++ defaults ++ unknown ++ unordered ++ mixed
      }
      patches.flatten.asPatch
    }
  }
}

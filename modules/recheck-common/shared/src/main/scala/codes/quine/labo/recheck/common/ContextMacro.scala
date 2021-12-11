package codes.quine.labo.recheck.common

import scala.reflect.macros.blackbox

private[common] object ContextMacro {
  def interrupt(c: blackbox.Context)(body: c.Tree): c.Tree = {
    import c.universe._

    val ctx = c.prefix

    val pos = c.enclosingPosition
    val path = pos.source.path.replaceFirst("\\A.*/(?=modules)", "")
    val source = Literal(Constant(s"$path:${pos.line}"))

    val isCancelled = q"$ctx.token != null && $ctx.token.isCancelled()"
    val isInterrupted = q"$isCancelled || $ctx.deadline != null && $ctx.deadline.isOverdue()"

    q"""
      if ($isInterrupted) {
        if ($isCancelled) throw new codes.quine.labo.recheck.common.CancelException($source)
        else throw new codes.quine.labo.recheck.common.TimeoutException($source)
      }
      $body
    """
  }
}

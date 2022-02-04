package codes.quine.labs.recheck.common

import scala.reflect.macros.blackbox

/** ContextMacro is an object holds [[Context]] method implementations. */
private[common] object ContextMacro {

  /** Returns a string literal tree represents caller location. */
  def sourceLiteral(c: blackbox.Context): c.Tree = {
    import c.universe._

    val pos = c.enclosingPosition
    val path = pos.source.path.replaceFirst("\\A.*/(?=modules)", "")
    val source = Literal(Constant(s"$path:${pos.line}"))

    source
  }

  /** [[Context#interrupt]] implementation. */
  def interrupt(c: blackbox.Context)(body: c.Tree): c.Tree = {
    import c.universe._

    val ctx = c.prefix
    val source = sourceLiteral(c)
    val isCancelled = q"$ctx.token != null && $ctx.token.isCancelled()"
    val isInterrupted = q"$isCancelled || $ctx.deadline != null && $ctx.deadline.isOverdue()"

    q"""
      if ($isInterrupted) {
        if ($isCancelled) throw new codes.quine.labs.recheck.common.CancelException($source)
        else throw new codes.quine.labs.recheck.common.TimeoutException($source)
      }
      $body
    """
  }

  /** [[Context#log]] implementation. */
  def log(c: blackbox.Context)(message: c.Tree): c.Tree = {
    import c.universe._

    val ctx = c.prefix

    q"""
      if ($ctx.hasLogger) $ctx.logger($message)
    """
  }
}

package codes.quine.labs.resyntax.ast

/** Reference is a capture reference for back-reference. */
sealed abstract class Reference extends Product with Serializable

object Reference {

  /** BaseReference is a base class for a base reference of leveled reference. */
  sealed abstract class BaseReference extends Reference

  /** IndexedReference is `\1` style reference. */
  final case class IndexedReference(index: Int) extends BaseReference

  /** NamedReference is `\k<x>` style reference. */
  final case class NamedReference(name: String) extends BaseReference

  /** RelativeReference is `\g{-1}` style reference. */
  final case class RelativeReference(n: Int) extends BaseReference

  /** LeveledReference is `\g{1+1}` style reference. */
  final case class LeveledReference(base: BaseReference, level: Int) extends Reference
}

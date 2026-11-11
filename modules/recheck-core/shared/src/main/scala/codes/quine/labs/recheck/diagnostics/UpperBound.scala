package codes.quine.labs.recheck
package diagnostics

/** UpperBound is the result of the check by an upper bound of the matching time complexity, which the fuzz checker runs
  * before fuzzing: the complexity of the bound, linear or vulnerable, and the hotspot of its worst witness.
  */
final case class UpperBound(complexity: AttackComplexity, hotspot: Hotspot)

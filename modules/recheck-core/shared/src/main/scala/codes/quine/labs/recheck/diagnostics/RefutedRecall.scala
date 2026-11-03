package codes.quine.labs.recheck
package diagnostics

/** RefutedRecall is an attack found by a checker that recall validation refuted. */
final case class RefutedRecall(
    complexity: AttackComplexity.Vulnerable,
    attack: AttackPattern,
    hotspot: Hotspot,
    recallValidation: RecallValidation
)

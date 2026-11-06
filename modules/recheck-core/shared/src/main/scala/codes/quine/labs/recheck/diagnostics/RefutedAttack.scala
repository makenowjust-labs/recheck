package codes.quine.labs.recheck
package diagnostics

/** RefutedAttack is an attack found by a checker that runtime-attack verification refuted. */
final case class RefutedAttack(
    complexity: AttackComplexity.Vulnerable,
    attack: AttackPattern,
    hotspot: Hotspot,
    runtimeAttack: RuntimeAttack
)

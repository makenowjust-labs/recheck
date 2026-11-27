package codes.quine.labs.recheck.verification

import scala.collection.mutable

import codes.quine.labs.recheck.context.Context
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.DiagnosticsState
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.diagnostics.RefutedAttack
import codes.quine.labs.recheck.diagnostics.RuntimeAttack
import codes.quine.labs.recheck.exception.RuntimeAttacksRefutedException
import codes.quine.labs.recheck.parameters.AttackStringCharacteristics
import codes.quine.labs.recheck.verification.RuntimeAttacker.AttackKey
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.ir.alphabet.PatternAlphabet

/** RuntimeAttacker runs the attacks of a checking on the runtime, and remembers the ones that the runtime refutes. */
final class RuntimeAttacker(
    state: DiagnosticsState,
    execute: AttackPattern => Option[RuntimeAttack],
    readStarts: Set[SourceLocation],
    sets: Seq[CharSet],
    mode: UnitMode,
    // Parameters:
    limit: Int,
    maxSize: Int,
    maxRefutedAttacks: Int
)(using ctx: Context):

  /** The keys of the attacks that the runtime refuted. */
  private val refutedKeys: mutable.Set[AttackKey] = mutable.Set.empty

  /** Whether an attack of the complexity, the string and the hotspot is the same as a refuted one (see [[AttackKey]]).
    */
  def isRefuted(complexity: AttackComplexity.Vulnerable, attack: AttackPattern, hotspot: Hotspot): Boolean =
    refutedKeys.contains(AttackKey(complexity, attack, hotspot, readStarts, sets))

  /** Runs the attack on the runtime, and returns its vulnerable diagnostics unless it is refuted. */
  def attack(
      complexity: AttackComplexity.Vulnerable,
      attack: AttackPattern,
      hotspot: Hotspot
  ): Option[Diagnostics.Vulnerable] =
    val adjusted = attack.adjust(complexity, mode, limit, maxSize)
    val key = AttackKey(complexity, adjusted, hotspot, readStarts, sets)
    if refutedKeys.contains(key) then
      ctx.log("runtime attack: skip the same attack as a refuted one")
      None
    else
      execute(adjusted) match
        case Some(runtimeAttack) if !runtimeAttack.isConfirmed =>
          state.refutedAttacks += RefutedAttack(complexity, adjusted, hotspot, runtimeAttack)
          refutedKeys += key
          if state.refutedAttacks.size >= maxRefutedAttacks then throw new RuntimeAttacksRefutedException
          None
        case runtimeAttack => Some(state.vulnerable(complexity, adjusted, hotspot, runtimeAttack))

object RuntimeAttacker:

  /** Returns the sets of characters that tell an attack string from another in [[AttackKey]]. */
  def characteristicSets(
      ir: IRPattern,
      // Parameters:
      attackStringCharacteristics: AttackStringCharacteristics
  ): Seq[CharSet] =
    def atoms: Seq[CharSet] = PatternAlphabet.of(ir).pairs.map(_._1)

    attackStringCharacteristics match
      case AttackStringCharacteristics.None      => Seq.empty
      case AttackStringCharacteristics.Latin1    => Seq(ir.universe.diff(CharSet.range(0x00, 0xff)))
      case AttackStringCharacteristics.CharClass =>
        val literals = ir.literalSets
        atoms.filterNot(atom => literals.exists(atom.subsetOf))
      case AttackStringCharacteristics.Alphabet => atoms

  /** AttackKey identifies an attack by the degree of the polynomial complexity (or `None` for the exponential one), the
    * spots of the hotspot and the characteristics of the attack string.
    *
    * The warm and cold spots tell apart the attacks that grow at the same hot spots through different parts of the
    * pattern, one of which an engine may refute while it confirms another (e.g. `x[^∆]*∆y` on `'x'.repeat(n)`, which V8
    * refutes as a Latin-1 string, and on a string with `∆`).
    *
    * The spots are the ones at `readStarts`, the locations where the engine starts to read the pattern (see
    * [[IRPattern.readStartLocations]]), which have a part of the pattern as its characters read first up to
    * `maxRefutedAttackPartSize`. The attacks that differ only in how many characters of a part they read after them are
    * the same one (e.g. `form.*edit_profile` on the repetitions of `formedit` and of `formedit_p`).
    *
    * The characteristics are the indices of `sets` (see [[characteristicSets]]) that have a character of the attack
    * string.
    */
  private[recheck] final case class AttackKey(
      degree: Option[Int],
      spots: Seq[Hotspot.Spot],
      characteristics: Set[Int]
  )

  private[recheck] object AttackKey:

    def apply(
        complexity: AttackComplexity.Vulnerable,
        attack: AttackPattern,
        hotspot: Hotspot,
        readStarts: Set[SourceLocation],
        sets: Seq[CharSet]
    ): AttackKey =
      val chars = (attack.pumps.iterator.flatMap((s, t, _) => s.iterator ++ t.iterator) ++ attack.suffix.iterator).toSet
      val characteristics = sets.indices.filter(i => chars.exists(sets(i).contains)).toSet
      val degree = complexity match
        case AttackComplexity.Polynomial(degree, _) => Some(degree)
        case AttackComplexity.Exponential(_)        => None
      val spots =
        for
          loc <- readStarts.toSeq.sortBy(loc => (loc.start, loc.end))
          spot <- hotspot.spots.find(spot => spot.start <= loc.start && loc.end <= spot.end)
        yield Hotspot.Spot(loc.start, loc.end, spot.temperature)
      AttackKey(degree, spots, characteristics)

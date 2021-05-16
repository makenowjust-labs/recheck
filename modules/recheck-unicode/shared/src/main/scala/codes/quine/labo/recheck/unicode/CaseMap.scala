package codes.quine.labo.recheck.unicode

/** Utilities for Unicode case mapping/folding.
  *
  * The methods defined here are designed for implementing ignore-case of regexp.
  */
private[unicode] object CaseMap {

  /** Conversion is a pair of a conversion domain and a conversion offset. */
  final case class Conversion(domain: IntervalSet[UChar], offset: Int)

  /** Upper case conversion mappings.
    *
    * They are useful to implement ignore-case on non-Unicode regexp.
    */
  lazy val Upper: Seq[Conversion] = CaseMapData.Upper

  /** Fold case conversion mappings.
    *
    * They are useful to implement ignore-case on Unicode regexp.
    */
  lazy val Fold: Seq[Conversion] = CaseMapData.Fold
}

package codes.quine.labo.redos

import data.IChar

/** Witness is a witness, which is a pump string with suffix.
  *
  * For example, when a witness object forms `Witness(Seq((x, y), (z, w)), u)`,
  * an actual witness string is `x y^n z w^n u` for any integer `n`.
  */
final case class Witness(pump: Seq[(Seq[IChar], Seq[IChar])], suffix: Seq[IChar])

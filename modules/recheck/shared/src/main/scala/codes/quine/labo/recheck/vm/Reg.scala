package codes.quine.labo.recheck.vm

/** Reg is a register reference. */
final case class Reg(index: Int) {
  override def toString: String = s"%$index"
}

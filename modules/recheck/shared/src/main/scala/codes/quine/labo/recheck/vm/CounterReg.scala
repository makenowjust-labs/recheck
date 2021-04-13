package codes.quine.labo.recheck.vm

/** CounterReg is a register reference for a counter. */
final case class CounterReg(index: Int) {
  override def toString: String = s"%$index"
}

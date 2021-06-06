package codes.quine.labo.recheck.vm

/** Label is a reference to a block in a program.
  *
  * It has a name string and unique ID.
  */
final case class Label(name: String, index: Int) {

  /** A reference to a block referred by this.
    *
    * This is assigned by ProgramBuilder, so it may be `null` on building a program.
    */
  private[this] var _block: Block = _

  /** A reference to a block referred by this. */
  def block: Block = _block

  /** Sets a block reference. */
  private[vm] def block_=(block: Block): Unit = {
    this._block = block
  }

  override def toString: String = s"#$name@$index"
}

package codes.quine.labs.recheck.vm

/** Block is a basic block of a program. */
final case class Block(insts: IArray[Inst.NonTerminator], terminator: Inst.Terminator):

  override def toString: String =
    (insts.iterator ++ Iterator(terminator)).map(_.toString).mkString("\n")

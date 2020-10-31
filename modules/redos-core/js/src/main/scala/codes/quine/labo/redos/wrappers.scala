package codes.quine.labo.redos

import scalajs.js
import scalajs.js.JSConverters._
import automaton.Complexity
import automaton.Witness
import data.IChar

/** DiagnosticsJS is a JS wrapper for Diagnostics. */
trait DiagnosticsJS extends js.Object {

  /** A status of this diagnostics. One of `safe`, `vulnerable` and `unknown`. */
  def status: String

  /** An attack string. It is available on `vulnerable` diagnostics. */
  def attack: js.UndefOr[String]

  /** A matching-time complexity. It is available on `safe` or `vulnerable` diagnostics. */
  def complexity: js.UndefOr[ComplexityJS]

  /** An error kind. It is available on `unknown` diagnostics. */
  def error: js.UndefOr[ErrorKindJS]
}

/** DiagnosticsJS utilities. */
object DiagnosticsJS {

  /** Constructs a DiagnosticsJS from the actual Diagnostics. */
  def from(d: Diagnostics): DiagnosticsJS = d match {
    case Diagnostics.Safe(c) =>
      js.Dynamic
        .literal(status = "safe", complexity = c.map(ComplexityJS.from(_)).orUndefined)
        .asInstanceOf[DiagnosticsJS]
    case Diagnostics.Vulnerable(a, c) =>
      js.Dynamic
        .literal(
          status = "vulnerable",
          attack = IStringJS.from(a),
          complexity = c.map(ComplexityJS.from(_)).orUndefined
        )
        .asInstanceOf[DiagnosticsJS]
    case Diagnostics.Unknown(k) =>
      js.Dynamic.literal(status = "unknown", error = ErrorKindJS.from(k)).asInstanceOf[DiagnosticsJS]
  }
}

/** ComplexityJS is a JS wrapper for Complexity. */
trait ComplexityJS extends js.Object {

  /** A type of this complexity. One of `constant`, `linear`, `exponential` and `polynomial`. */
  def `type`: String

  /** A polynomial complexity's degree. It is available on `polynomial` complexity. */
  def degree: js.UndefOr[Int]

  /** A witness pattern. It is available on `exponential` or `polynomial` complexity. */
  def witness: js.UndefOr[WitnessJS]
}

/** ComplexityJS utilities. */
object ComplexityJS {

  /** Constructs a ComplexityJS from the actual Complexity. */
  def from(c: Complexity[IChar]): ComplexityJS = c match {
    case Complexity.Constant => js.Dynamic.literal(`type` = "constant").asInstanceOf[ComplexityJS]
    case Complexity.Linear   => js.Dynamic.literal(`type` = "linear").asInstanceOf[ComplexityJS]
    case Complexity.Polynomial(d, w) =>
      js.Dynamic.literal(`type` = "polynomial", degree = d, witness = WitnessJS.from(w)).asInstanceOf[ComplexityJS]
    case Complexity.Exponential(w) =>
      js.Dynamic.literal(`type` = "exponential", witness = WitnessJS.from(w)).asInstanceOf[ComplexityJS]
  }
}

/** WitnessJS is a JS wrapper for Witness. */
trait WitnessJS extends js.Object {

  /** Pump objects. */
  def pumps: js.Array[PumpJS]

  /** A suffix string. */
  def suffix: String
}

/** WitnessJS utilities. */
object WitnessJS {

  /** Constructs a WitnessJS from the actual Witness. */
  def from(w: Witness[IChar]): WitnessJS =
    js.Dynamic
      .literal(pumps = w.pumps.map(PumpJS.from(_)).toJSArray, suffix = IStringJS.from(w.suffix))
      .asInstanceOf[WitnessJS]
}

/** PumpJS is a JS wrapper for Pump. */
trait PumpJS extends js.Object {

  /** A prefix string. */
  def prefix: String

  /** A pump string. */
  def pump: String
}

/** PumpJS utilities. */
object PumpJS {

  /** Constructs a PumpJS from the actual Pump. */
  def from(p: (Seq[IChar], Seq[IChar])): PumpJS =
    js.Dynamic.literal(prefix = IStringJS.from(p._1), pump = IStringJS.from(p._2)).asInstanceOf[PumpJS]
}

/** IString (`Seq[IChar]`) utilities. */
object IStringJS {

  /** Constructs a Strinig from the actual IString. */
  def from(seq: Seq[IChar]): String = seq.map(_.head.asString).mkString
}

/** ErrorKindJS is a JS wrapper for ErrorKind. */
trait ErrorKindJS extends js.Object {

  /** An error kind name. One of `timeout`, `unsupported` and `invalid`. */
  def kind: String

  /** An error message. It is available on `unsupported` and `invalid` error. */
  def message: js.UndefOr[String]
}

/** ErrorKindJS utilities. */
object ErrorKindJS {

  /** Constructs a ErrorKindJS from the actual ErrorKind. */
  def from(k: Diagnostics.ErrorKind): ErrorKindJS = k match {
    case Diagnostics.ErrorKind.Timeout => js.Dynamic.literal(kind = "timeout").asInstanceOf[ErrorKindJS]
    case Diagnostics.ErrorKind.Unsupported(msg) =>
      js.Dynamic.literal(kind = "unsupported", message = msg).asInstanceOf[ErrorKindJS]
    case Diagnostics.ErrorKind.InvalidRegExp(msg) =>
      js.Dynamic.literal(kind = "invalid", message = msg).asInstanceOf[ErrorKindJS]
  }
}

package codes.quine.labo.recheck.common

/** AccelerationMode is an enum to specify a mode of acceleration of VM execution on fuzzing. */
sealed abstract class AccelerationMode extends Product with Serializable

object AccelerationMode {

  /** Auto is `auto` mode. */
  case object Auto extends AccelerationMode

  /** On is force `on` mode. */
  case object On extends AccelerationMode

  /** Off is force `off` mode. */
  case object Off extends AccelerationMode
}

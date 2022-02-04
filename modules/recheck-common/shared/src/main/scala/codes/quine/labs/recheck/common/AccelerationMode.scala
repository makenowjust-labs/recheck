package codes.quine.labs.recheck.common

/** AccelerationMode is an enum to specify a mode of acceleration of VM execution on fuzzing. */
sealed abstract class AccelerationMode extends Product with Serializable

object AccelerationMode {

  /** Auto is `auto` mode. */
  case object Auto extends AccelerationMode {
    override def toString: String = "auto"
  }

  /** On is force `on` mode. */
  case object On extends AccelerationMode {
    override def toString: String = "on"
  }

  /** Off is force `off` mode. */
  case object Off extends AccelerationMode {
    override def toString: String = "off"
  }
}

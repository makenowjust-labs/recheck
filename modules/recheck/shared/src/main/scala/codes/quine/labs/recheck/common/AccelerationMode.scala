package codes.quine.labs.recheck.common

/** AccelerationMode is an enum to specify a mode of acceleration of VM execution on fuzzing. */
enum AccelerationMode:

  /** Auto means `auto` mode. */
  case Auto

  /** On means to force `on` mode. */
  case On

  /** Off means to force `off` mode. */
  case Off

  override def toString: String = this match
    case Auto => "auto"
    case On   => "on"
    case Off  => "off"

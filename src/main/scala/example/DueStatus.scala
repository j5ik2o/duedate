package example

sealed trait DueStatus

object DueStatus {
  case object Active extends DueStatus
  case object Due extends DueStatus
  case object Completed extends DueStatus
  case object Canceled extends DueStatus
}

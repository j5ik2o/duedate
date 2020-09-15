package example

sealed trait TaskStatus

object TaskStatus {
  case object Active extends TaskStatus
  case object Due extends TaskStatus
  case object Completed extends TaskStatus
  case object Canceled extends TaskStatus
}

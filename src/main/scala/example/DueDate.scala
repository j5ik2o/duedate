package example

import java.time.{Instant, LocalDate, ZoneOffset}

object DueDate {
  def apply(dueDate: LocalDate, zoneOffset: ZoneOffset): DueDate =
    new DueDate(dueDate, zoneOffset: ZoneOffset)
  def unapply(self: DueDate): Option[LocalDate] = Some(self.toLocalDate)
}

final case class DueDate(dueDate: LocalDate, zoneOffset: ZoneOffset) {
  private val dueDateInstant = dueDate.atStartOfDay().toInstant(zoneOffset)

  def toLocalDate: LocalDate = dueDate

  def isDue: Boolean = {
    Instant
      .now()
      .isAfter(dueDateInstant)
  }
}

package example.single

import java.time.{Instant, LocalDate, ZoneOffset}

import example.DueStatus

object DueDate {
  def apply(dueDate: LocalDate, zoneOffset: ZoneOffset): DueDate =
    new DueDate(dueDate, zoneOffset: ZoneOffset, DueStatus.Active)
  def unapply(self: DueDate): Option[LocalDate] = Some(self.toLocalDate)
}

final class DueDate private (dueDate: LocalDate,
                             zoneOffset: ZoneOffset,
                             val status: DueStatus) {
  private val dueDateInstant = dueDate.atStartOfDay().toInstant(zoneOffset)

  def toLocalDate: LocalDate = dueDate

  private def isDue: Boolean = {
    Instant
      .now()
      .isAfter(dueDateInstant)
  }

  def confirm(listener: Option[DueDate => Unit] = None): DueDate = {
    if (status == DueStatus.Active && isDue) {
      val newDueDate = new DueDate(dueDate, zoneOffset, DueStatus.Due)
      listener.foreach(_(newDueDate))
      newDueDate
    } else
      this
  }

  def complete(listener: Option[DueDate => Unit] = None): DueDate = {
    if (status == DueStatus.Active || status == DueStatus.Due) {
      val newDueDate = new DueDate(dueDate, zoneOffset, DueStatus.Completed)
      listener.foreach(_(newDueDate))
      newDueDate
    } else
      this
  }

  def cancel(listener: Option[DueDate => Unit] = None): DueDate = {
    if (status == DueStatus.Active) {
      val newDueDate = new DueDate(dueDate, zoneOffset, DueStatus.Canceled)
      listener.foreach(_(newDueDate))
      newDueDate
    } else
      this
  }

  override def equals(other: Any): Boolean = other match {
    case that: DueDate =>
      dueDateInstant == that.dueDateInstant &&
        status == that.status
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(dueDateInstant, status)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString = s"DueDate($status, $toLocalDate)"
}

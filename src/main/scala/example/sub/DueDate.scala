package example.sub

import java.time.{Instant, LocalDate, OffsetDateTime, ZoneOffset}

import example.DueStatus

sealed trait DueDate {
  protected def dueDate: LocalDate
  protected def zoneOffset: ZoneOffset
  private def dueDateInstant: OffsetDateTime =
    dueDate.atStartOfDay().atOffset(zoneOffset)

  def status: DueStatus

  protected def isDue: Boolean = {
    Instant
      .now()
      .isAfter(Instant.from(dueDateInstant))
  }

  def confirm(listener: Option[DueDate => Unit] = None): DueDate
  def complete(listener: Option[DueDate => Unit] = None): DueDate
  def cancel(listener: Option[DueDate => Unit] = None): DueDate

  def toLocalDate: LocalDate = dueDate

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

object DueDate {
  def apply(dueDate: LocalDate, zoneOffset: ZoneOffset): DueDate =
    Active(dueDate, zoneOffset)

  def unapply(self: DueDate): Option[LocalDate] = Some(self.toLocalDate)

  final case class Active private[sub] (protected val dueDate: LocalDate,
                                        protected val zoneOffset: ZoneOffset)
      extends DueDate {
    override def status: DueStatus = DueStatus.Active

    override def confirm(listener: Option[DueDate => Unit]): DueDate = {
      if (isDue) {
        val newDueDate = Due(dueDate, zoneOffset)
        listener.foreach(_(newDueDate))
        newDueDate
      } else
        this
    }

    override def complete(listener: Option[DueDate => Unit]): DueDate = {
      val newDueDate = Completed(dueDate, zoneOffset)
      listener.foreach(_(newDueDate))
      newDueDate
    }

    override def cancel(listener: Option[DueDate => Unit]): DueDate = {
      val newDueDate = Canceled(dueDate, zoneOffset)
      listener.foreach(_(newDueDate))
      newDueDate
    }
  }

  final case class Due private[sub] (protected val dueDate: LocalDate,
                                     protected val zoneOffset: ZoneOffset)
      extends DueDate {
    override def status: DueStatus = DueStatus.Due

    override def confirm(listener: Option[DueDate => Unit]): DueDate = this

    override def complete(listener: Option[DueDate => Unit]): DueDate = {
      val newDueDate = Completed(dueDate, zoneOffset)
      listener.foreach(_(newDueDate))
      newDueDate
    }

    override def cancel(listener: Option[DueDate => Unit]): DueDate = this
  }

  final case class Completed private[sub] (protected val dueDate: LocalDate,
                                           protected val zoneOffset: ZoneOffset)
      extends DueDate {
    override def status: DueStatus = DueStatus.Completed

    override def confirm(listener: Option[DueDate => Unit]): DueDate = this
    override def complete(listener: Option[DueDate => Unit]): DueDate = this
    override def cancel(listener: Option[DueDate => Unit]): DueDate = this
  }

  final case class Canceled private[sub] (protected val dueDate: LocalDate,
                                          protected val zoneOffset: ZoneOffset)
      extends DueDate {
    override def status: DueStatus = DueStatus.Canceled

    override def confirm(listener: Option[DueDate => Unit]): DueDate = this
    override def complete(listener: Option[DueDate => Unit]): DueDate = this
    override def cancel(listener: Option[DueDate => Unit]): DueDate = this
  }
}

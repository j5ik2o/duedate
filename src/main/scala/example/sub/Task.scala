package example.sub

import java.time.ZoneOffset

import example.{DueDate, EmployeeId, TaskDescription, TaskId, TaskStatus}

sealed trait Task {

  def id: TaskId
  def status: TaskStatus
  def assignee: EmployeeId
  def assigner: EmployeeId
  def description: TaskDescription
  def dueDate: DueDate

  def confirm(listener: Option[Task => Unit] = None): Task
  def complete(listener: Option[Task => Unit] = None): Task
  def cancel(listener: Option[Task => Unit] = None): Task

  def toDueDate: DueDate = dueDate

}

object Task {

  def apply(id: TaskId,
            assignee: EmployeeId,
            assigner: EmployeeId,
            description: TaskDescription,
            dueDate: DueDate): Task =
    Active(id, assignee, assigner, description, dueDate)

  final case class Active private[sub] (id: TaskId,
                                        assignee: EmployeeId,
                                        assigner: EmployeeId,
                                        description: TaskDescription,
                                        dueDate: DueDate)
      extends Task {
    override def status: TaskStatus = TaskStatus.Active

    override def confirm(listener: Option[Task => Unit]): Task = {
      if (dueDate.isDue) {
        val newTask =
          Due(id, assignee, assigner, description, dueDate)
        listener.foreach(_(newTask))
        newTask
      } else
        this
    }

    override def complete(listener: Option[Task => Unit]): Task = {
      val newTask =
        Completed(id, assignee, assigner, description, dueDate)
      listener.foreach(_(newTask))
      newTask
    }

    override def cancel(listener: Option[Task => Unit]): Task = {
      val newTask =
        Canceled(id, assignee, assigner, description, dueDate)
      listener.foreach(_(newTask))
      newTask
    }
  }

  final case class Due private[sub] (id: TaskId,
                                     assignee: EmployeeId,
                                     assigner: EmployeeId,
                                     description: TaskDescription,
                                     dueDate: DueDate)
      extends Task {
    override def status: TaskStatus = TaskStatus.Due

    override def confirm(listener: Option[Task => Unit]): Task = this

    override def complete(listener: Option[Task => Unit]): Task = {
      val newTask =
        Completed(id, assignee, assigner, description, dueDate)
      listener.foreach(_(newTask))
      newTask
    }

    override def cancel(listener: Option[Task => Unit]): Task = this
  }

  final case class Completed private[sub] (id: TaskId,
                                           assignee: EmployeeId,
                                           assigner: EmployeeId,
                                           description: TaskDescription,
                                           dueDate: DueDate)
      extends Task {
    override def status: TaskStatus = TaskStatus.Completed

    override def confirm(listener: Option[Task => Unit]): Task = this
    override def complete(listener: Option[Task => Unit]): Task = this
    override def cancel(listener: Option[Task => Unit]): Task = this
  }

  final case class Canceled private[sub] (id: TaskId,
                                          assignee: EmployeeId,
                                          assigner: EmployeeId,
                                          description: TaskDescription,
                                          dueDate: DueDate)
      extends Task {
    override def status: TaskStatus = TaskStatus.Canceled

    override def confirm(listener: Option[Task => Unit]): Task = this
    override def complete(listener: Option[Task => Unit]): Task = this
    override def cancel(listener: Option[Task => Unit]): Task = this
  }
}

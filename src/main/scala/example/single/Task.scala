package example.single

import java.time.ZoneOffset

import example.{DueDate, EmployeeId, TaskDescription, TaskId, TaskStatus}

final case class Task(id: TaskId,
                      status: TaskStatus,
                      assignee: EmployeeId,
                      assigner: EmployeeId,
                      description: TaskDescription,
                      dueDate: DueDate,
                      zoneOffset: ZoneOffset) {

  def confirm(listener: Option[Task => Unit] = None): Task = {
    if (status == TaskStatus.Active && dueDate.isDue) {
      val newTask = copy(status = TaskStatus.Due)
      listener.foreach(_(newTask))
      newTask
    } else
      this
  }

  def complete(listener: Option[Task => Unit] = None): Task = {
    if (status == TaskStatus.Active || status == TaskStatus.Due) {
      val newTask = copy(status = TaskStatus.Completed)
      listener.foreach(_(newTask))
      newTask
    } else
      this
  }

  def cancel(listener: Option[Task => Unit] = None): Task = {
    if (status == TaskStatus.Active) {
      val newTask = copy(status = TaskStatus.Canceled)
      listener.foreach(_(newTask))
      newTask
    } else
      this
  }
}

package example.sub

import java.time.{LocalDate, ZoneOffset}

import example.{DueDate, EmployeeId, TaskDescription, TaskId, TaskStatus}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DueDateSpec extends AnyFreeSpec with Matchers {
  "DueDate" - {
    "Status is confirmed as Active" in {
      val dueDate = DueDate(LocalDate.now().plusDays(1), ZoneOffset.UTC)
      val task = Task(
        TaskId(1L),
        EmployeeId(1L),
        EmployeeId(1L),
        TaskDescription("Test"),
        dueDate
      )
      task.confirm() match {
        case Task.Active(_, _, _, _, _) =>
        case _                          => fail()
      }
    }
    "Status is confirmed as Due" in {
      val dueDate = DueDate(LocalDate.now(), ZoneOffset.UTC)
      val task = Task(
        TaskId(1L),
        EmployeeId(1L),
        EmployeeId(1L),
        TaskDescription("Test"),
        dueDate
      )
      task.confirm() match {
        case Task.Due(_, _, _, _, _) =>
        case _                       => fail()
      }
    }
    "Status is confirmed as Completed" in {
      val dueDate = DueDate(LocalDate.now(), ZoneOffset.UTC)
      val task = Task(
        TaskId(1L),
        EmployeeId(1L),
        EmployeeId(1L),
        TaskDescription("Test"),
        dueDate
      )
      task.confirm().complete() match {
        case Task.Completed(_, _, _, _, _) =>
        case _                             => fail()
      }
    }
    "Status is confirmed as Canceled" in {
      val dueDate = DueDate(LocalDate.now(), ZoneOffset.UTC)
      val task = Task(
        TaskId(1L),
        EmployeeId(1L),
        EmployeeId(1L),
        TaskDescription("Test"),
        dueDate
      )
      task.cancel() match {
        case Task.Canceled(_, _, _, _, _) =>
        case _                            => fail()
      }
    }
  }
}

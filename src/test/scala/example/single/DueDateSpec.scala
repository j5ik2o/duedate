package example.single

import java.time.{LocalDate, ZoneOffset}

import example.sub.Task
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
      task.confirm().status shouldBe TaskStatus.Active
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
      task.confirm().status shouldBe TaskStatus.Due
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
      task.confirm().complete().status shouldBe TaskStatus.Completed
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
      task.cancel().status shouldBe TaskStatus.Canceled
    }
  }
}

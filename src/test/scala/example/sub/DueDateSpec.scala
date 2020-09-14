package example.sub

import java.time.{LocalDate, ZoneOffset}

import example.DueStatus
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DueDateSpec extends AnyFreeSpec with Matchers {
  "DueDate" - {
    "Status is confirmed as Active" in {
      val dueDate = DueDate(LocalDate.now().plusDays(1), ZoneOffset.UTC)
      dueDate.confirm() match {
        case DueDate.Active(_, _) =>
        case _                    => fail()
      }
    }
    "Status is confirmed as Due" in {
      val dueDate = DueDate(LocalDate.now(), ZoneOffset.UTC)
      dueDate.confirm() match {
        case DueDate.Due(_, _) =>
        case _                 => fail()
      }
    }
    "Status is confirmed as Completed" in {
      val dueDate = DueDate(LocalDate.now(), ZoneOffset.UTC)
      dueDate.confirm().complete() match {
        case DueDate.Completed(_, _) =>
        case _                       => fail()
      }

    }
    "Status is confirmed as Canceled" in {
      val dueDate = DueDate(LocalDate.now(), ZoneOffset.UTC)
      dueDate.cancel() match {
        case DueDate.Canceled(_, _) =>
        case _                      => fail()
      }
    }
  }
}

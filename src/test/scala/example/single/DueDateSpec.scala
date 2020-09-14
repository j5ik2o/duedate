package example.single

import java.time.{LocalDate, ZoneOffset}

import example.DueStatus
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DueDateSpec extends AnyFreeSpec with Matchers {
  "DueDate" - {
    "Status is confirmed as Active" in {
      val dueDate = DueDate(LocalDate.now().plusDays(1), ZoneOffset.UTC)
      dueDate.confirm().status shouldBe DueStatus.Active
    }
    "Status is confirmed as Due" in {
      val dueDate = DueDate(LocalDate.now(), ZoneOffset.UTC)
      dueDate.confirm().status shouldBe DueStatus.Due
    }
    "Status is confirmed as Completed" in {
      val dueDate = DueDate(LocalDate.now(), ZoneOffset.UTC)
      dueDate.confirm().complete().status shouldBe DueStatus.Completed
    }
    "Status is confirmed as Canceled" in {
      val dueDate = DueDate(LocalDate.now(), ZoneOffset.UTC)
      dueDate.cancel().status shouldBe DueStatus.Canceled
    }
  }
}

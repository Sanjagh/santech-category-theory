package cat
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.concurrent.Future
import scala.concurrent.Await

class SemigroupTest extends FlatSpec with Matchers {
  "String semigroup" should "be associative" in {
    import instances._

    ("a" <> "b") <> "c" shouldBe "abc"
    ("a" <> ("b" <> "c")) shouldBe "abc"
  }

  "future semigroup" should "work with successful future" in {
    val f1: Future[Int] = Future.successful(1)
    val f2: Future[Int] = Future.successful(2)
    import instances._

    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global

    Await.result(f1 <> f2, 2.seconds) shouldBe 3
  }

  "future semigroup" should "work with async future" in {
    import scala.concurrent.ExecutionContext.Implicits.global
    val f1: Future[Int] = Future {
      Thread.sleep(100)
      1
    }
    val f2: Future[Int] = Future {
      2
    }

    import instances._

    import scala.concurrent.duration._

    Await.result(f1 <> f2, 2.seconds) shouldBe 3
  }

  "Map semigroup" should "(inclusive) merge two maps" in {
    import instances._
    val map1 = Map(
      "a" -> 1,
      "b" -> 2
    )
    val map2 = Map(
      "b" -> 3,
      "c" -> 1
    )

    val map3 = Map(
      "a" -> 1,
      "b" -> 5,
      "c" -> 1
    )

    (map1 <> map2) shouldBe map3

  }

  "Future map merger" should "merge two list of click-urls" in {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    import instances.{
      SemigroupSyntax,
      futureSemigroup,
      mapSemigroup,
      intSemigroup2
    }
    implicit val is = intSemigroup2
//    import instances._

    val f1 = Future {
      Map(
        "google.com" -> 5,
        "sanjagh.pro" -> 1000
      )
    }

    val f2 = Future {
      Map(
        "google.com" -> 100,
        "facebook.com" -> 20
      )
    }

    Await.result(f1 <> f2, 2.seconds) shouldBe Map(
      "google.com" -> 105,
      "sanjagh.pro" -> 1000,
      "facebook.com" -> 20
    )

  }

  "OptionMonoid" should "support associativity" in {

    import monoidInstances._
    import instances.OptionHelper
    import instances.SemigroupSyntax

    val none : Option[Int] = None

    (optionMonoid[Int].pure <> (3.some))  shouldBe 3.some
    (optionMonoid[Int].pure <> none) shouldBe none

  
  }

}

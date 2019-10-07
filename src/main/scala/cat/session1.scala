package cat
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

sealed trait Semigroup[T] {
  def operation(x: T, y: T): T
}

sealed trait Monoid[T] extends Semigroup[T] {

  def pure: T
}
object instances {

  implicit class OptionHelper[T](t: T) {
    def some: Option[T] = Some(t)
  }

  implicit class SemigroupSyntax[T](first: T) {
    def <>(second: T)(implicit semigroup: Semigroup[T]): T =
      semigroup.operation(first, second)
  }

  implicit val intSemigroup = new Semigroup[Int] {
    override def operation(x: Int, y: Int): Int = {
      x + y
    }
  }

  val intSemigroup2 = new Semigroup[Int] {
    override def operation(x: Int, y: Int): Int = x * y
  }

  implicit val stringSemigroup = new Semigroup[String] {
    override def operation(x: String, y: String): String = {
      x + y
    }
  }

  implicit def optionSemigroup[T](implicit semigroup: Semigroup[T]) =
    new Semigroup[Option[T]] {
      override def operation(x: Option[T], y: Option[T]): Option[T] =
        (x, y) match {
          case (Some(first), None)         => first.some
          case (None, Some(second))        => second.some
          case (None, None)                => None
          case (Some(first), Some(second)) => (first <> second).some
        }
    }

  implicit def futureSemigroup[T](
      implicit semigroup: Semigroup[T],
      ec: ExecutionContext
  ) = new Semigroup[Future[T]] {
    override def operation(x: Future[T], y: Future[T]): Future[T] = {
      for {
        xValue <- x
        yValue <- y
      } yield xValue <> yValue
    }
  }

  implicit def mapSemigroup[K, V](implicit semigroup: Semigroup[V]) =
    new Semigroup[Map[K, V]] {
      override def operation(x: Map[K, V], y: Map[K, V]): Map[K, V] = {
        (x.keySet ++ y.keySet).map { key =>
          val xValue = x.get(key)
          val yValue = y.get(key)
          key -> (xValue <> yValue).get
        }.toMap
      }
    }
}

object monoidInstances {
  import instances.SemigroupSyntax
  def futureMonoid[T](implicit monoid: Monoid[T]) =
    new Monoid[Future[T]] {

      override def operation(x: Future[T], y: Future[T]): Future[T] = ???

      override def pure: Future[T] = Future.successful(monoid.pure)

    }

  implicit def optionMonoid[T](implicit monoid: Monoid[T]) =
    new Monoid[Option[T]] {

      override def operation(x: Option[T], y: Option[T]): Option[T] =
        (x, y) match {
          case (Some(l), Some(r)) => Some(l <> r)
          case (Some(l), None)    => Some(l)
          case (None, Some(r))    => Some(r)
          case (None, None)       => pure
        }

      override def pure: Option[T] = None
    }

  implicit val intMonoid = new Monoid[Int] {
    override def operation(x: Int, y: Int): Int = x + y
    override def pure: Int = 0
  }
}

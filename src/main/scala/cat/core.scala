package cat
import scala.util.Try
import scala.util.Success
import scala.util.Failure

trait Functor[C[_]] {
  def map[T, U](container: C[T])(f: T => U): C[U]
  def pure[T](t: T): C[T]
}

sealed trait Maybe[+T]
case class Full[T](value: T) extends Maybe[T]
case object Empty extends Maybe[Nothing]

sealed trait Union[+L, +R]
case class Right[R](value: R) extends Union[Nothing, R]
case class Left[L](value: L) extends Union[L, Nothing]

object core {

  implicit val maybeFunctor = new Functor[Maybe] {

    override def map[T, U](container: Maybe[T])(f: T => U): Maybe[U] =
      container match {
        case Full(value) => Full(f(value))
        case Empty       => Empty
      }

    override def pure[T](t: T): Maybe[T] = Full(t)
  }

  type Result[T] = Union[String, T]
  implicit val unionFunctor = new Functor[Result] {
    override def map[T, U](container: Result[T])(f: T => U): Result[U] =
      container match {
        case Right(value) => Right(f(value))
        case Left(err)    => Left(err)
      }
    override def pure[T](t: T): Result[T] = Right(t)
  }
}

object UnionSample {
  import core.Result

  def parseInt(input: String): Result[Int] =
    Try {
      input.toInt
    }.toOption match {
      case Some(value) => Right(value)
      case None        => Left(s"Could not parse $input")
    }

  import opts.FunctorOpts
  import core.unionFunctor

  val o = parseInt("salam").map(_ + 10)
  val o2 = unionFunctor.map(parseInt("salam"))(_ + 10)

  val o3 = parseInt("salam") match {
    case Right(value) => Right(value + 10)
    case Left(err)    => Left(err)
  }
}

object SampleComposition  {
  val f: String => Int = ???
  val g: Int => Double = ???

  val h: String => Double = f andThen g
}

object UnionValidationSample {
  import core.unionFunctor
  import opts.FunctorOpts
  import core.Result
  case class Date(year: Int, month: Int, day: Int)

  val sample = "1397/06/07"

  def parse(input: String) : Result[Date] = {
    val o = checkStructure(input)
    //val o2 = o.flatmap(checkStrings)
    // val o3 = o.flatmap(checkBoundries)
    ???
  }

  def checkStructure(input: String): Result[List[String]] = {
    val parts = input.split("/")
    if (parts.length == 3) {
      Right(parts.toList)
    } else {
      Left("Bad pattern")
    }
  }

  def checkStrings(input: List[String]): Result[List[Int]] = {
    Try {
      input.map(_.toInt)
    } match {
      case Success(list) => Right(list)
      case Failure(e)    => Left(e.getMessage())
    }
  }

  def checkBoundries(input: List[Int]): Result[Date] = {
    input match {
      case year :: month :: day :: Nil
          if year > 0 && month > 0 && month <= 12 && day > 0 && day <= 31 =>
        Right(Date(year, month, day))
      case _ =>
        Left("Bad numbers")
    }
  }

}

object Sample {

  val badValue = test("aoeuaoue")

  import opts.FunctorOpts
  import core.maybeFunctor

  val result = core.maybeFunctor.map(badValue)(_ + 10)

  val result2 = badValue.map(_ + 10)

  def test(name: String): Maybe[Int] = {
    if (name.length > 5)
      Full(name.length)
    else
      Empty
  }

  test("MyName") match {
    case Full(value) => Full(value + 10)
    case Empty       => Empty
  }

}

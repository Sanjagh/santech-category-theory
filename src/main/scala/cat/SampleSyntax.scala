package cat

object SampleSyntax {


  val a: Int = 12
  val b = "hi"

  def test(x: Int, y: Int): Int = x + y
  
  //test(12, "Salam")

  class MyArray[T](t: T) {
  }

  val myArray = new MyArray[Int](2)

  def shout(name: String)(implicit mark: String): String = {
    name.toUpperCase + mark
  }

  
  shout("Name")("!")

  //implicit val secondParam = "2"
  implicit val exMark = "!"
  shout("Name")



  def myPrint(s: String, s2: String): Unit = {
    println("Here : " + s)
  }
  //implicit def listToInt[T](list: List[T]) : Int = list.size

  implicit def intToString(a: Int) : String = a.toString

  myPrint(12, List[Int](12, 22).size)  // myPrint(intToString(12))

}

import scala.io.Source

object Main {
  def main(args : Array[String]) = {
    var IntCodes =  Source.fromFile("day2-input").mkString.split(",").map(_.trim).map(_.toInt)
    traverse(IntCodes, 0).mkString(",").foreach(print)
    print('\n')
  }

  def traverse(IntCodes: Array[Int], i: Int): Array[Int] = IntCodes(i) match {
    case 99 => IntCodes
    case 1 => add(IntCodes, i)
    case 2 => mul(IntCodes, i)
  }
  
  def add(IntCodes: Array[Int], i: Int): Array[Int] = {
    val a = IntCodes(i+1)
    val b = IntCodes(i+2)
    val pos = IntCodes(i+3)
    IntCodes(pos) = IntCodes(a)+IntCodes(b)
    return traverse(IntCodes, i+4)
  }

  def mul(IntCodes: Array[Int], i: Int): Array[Int] = {
    val a = IntCodes(i+1)
    val b = IntCodes(i+2)
    val pos = IntCodes(i+3)
    IntCodes(pos) = IntCodes(a)*IntCodes(b)
    return traverse(IntCodes, i+4)
  }
}

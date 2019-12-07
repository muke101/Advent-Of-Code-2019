import scala.io.Source

object Main {
  def main(args : Array[String]) = {
    val memory = Source.fromFile("day2-input").mkString.split(",").map(_.trim).map(_.toInt)
    println(bruteForce(memory, 0))
  }

  def bruteForce(memory: Array[Int], i: Int): Int = {
    var mem = memory.clone
    mem(1) = i
    return compute(mem, 0) match  {
      case -1 if i < 99 => bruteForce(memory, i+1)
      case j if j != 0 => 100*i+j
      case _ => return 0
    }
  }

  def compute(memory: Array[Int], j: Int): Int =  {
    var mem = memory.clone
    mem(2) = j
    return traverse(mem, 0)(0) match  {
      case 19690720 => j
      case _ if j < 99 => compute(memory, j+1)
      case _ => -1
    }
  }

  def traverse(IntCodes: Array[Int], i: Int): Array[Int] = IntCodes(i) match {
    case 99 => IntCodes
    case 1 => add(IntCodes, i)
    case 2 => mul(IntCodes, i)
    case _ => IntCodes
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

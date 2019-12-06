import scala.io.Source
import scala.math.floor

object Main {
  def main(args: Array[String]) = {
    val result = (for (n <- Source.fromFile("day1-1-input").getLines) yield calFuel(n.toInt)).foldLeft(0)(_ + _)
    println(result)
  }

  def calFuel(n: Double) : Int = {
    return (floor(n/3) - 2).toInt
  }
}

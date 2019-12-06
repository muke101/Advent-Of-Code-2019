import scala.io.Source
import scala.math.floor

object Main {
  def main(args: Array[String]) = {
    val masses = Source.fromFile("day1-input").getLines.map(_.toDouble)
    println(masses.map(calFuel).foldLeft(0)(_ + _))
  }

  def calFuel(num: Double) : Int = {
    val n = floor(num/3) - 2 
    if (n > 0)  {
      return (n + calFuel(n)).toInt
    }
    else  {
      return 0
    }
  }
}

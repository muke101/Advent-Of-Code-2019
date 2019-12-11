import scala.io.Source
import scala.math
import scala.collection.mutable.Map

object Main {
  def main(args: Array[String]): Unit = {
    val wires = Source.fromFile(args(0)).getLines
    val wire1 = wires.next.split(",")
    val wire2 = wires.next.split(",")
    var grid = Map[List[Int], Int]().withDefaultValue(0)
    println(shortestDistance(plotCoords(plotCoords(grid,wire1,List(0,0)), wire2, List(0,0))))
    
  }

  def plotCoords(grid: Map[List[Int], Int], wire: Array[String], start: List[Int]): Map[List[Int], Int] = {
    if (wire.length > 0)  {
      val end = start.zip(parseMove(wire(0))).map{case (a,b) => a+b}
      List(start,end) match {
        case List(List(x1,y1),List(x2,y2)) if x1 == x2 => drawY(grid,y1,y2,x1)
        case List(List(x1,y1),List(x2,y2)) if y1 == y2 => drawX(grid,x1,x2,y1)
      }
      return plotCoords(grid, wire.slice(1,wire.length), end)
    }
    else  {
      return grid
    }
  }

  def parseMove(move: String): List[Int] =  {
    val direction = move(0)
    val distance = move.slice(1,move.length).toInt

    return direction match  {
      case 'R' => List(distance, 0)
      case 'L' => List(-distance, 0)
      case 'U' => List(0, distance)
      case 'D' => List(0, -distance)
    }
  }

  def drawX(grid: Map[List[Int], Int], start: Int, end: Int, linePos: Int) = {
        val range = (start, end) match  {
          case (x1,x2) if x1 < x2 => (start to end)
          case (x1,x2) if x1 > x2 => (end to start)
          case _ => (start to end)
        }
        range.toList.slice(1,range.length-1).map(x => grid.put(List(x,linePos),grid.getOrElseUpdate(List(x,linePos),0)+1))
  }

  def drawY(grid: Map[List[Int], Int], start: Int, end: Int, linePos: Int) = {
        val range = (start, end) match  {
          case (y1,y2) if y1 < y2 => (start to end)
          case (y1,y2) if y1 > y2 => (end to start)
          case _ => (start to end)
        }
        range.toList.slice(1,range.length-1).map(y => grid.put(List(linePos,y),grid.getOrElseUpdate(List(linePos,y),0)+1))
  }

  def shortestDistance(grid: Map[List[Int], Int]): Int =  {
    grid(List(0,0)) = 0 //also a dirty hack
    val keys = grid.keys.filter(grid(_) == 2)
    val absolute_keys = keys.map(_.map(math.abs(_)))
    return absolute_keys.map(_.sum).min
  }
}

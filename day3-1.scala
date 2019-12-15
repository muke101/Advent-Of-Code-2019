import scala.io.Source
import scala.math
import scala.collection.mutable.{Map,Set}

object Main {
  def main(args: Array[String]): Unit = {
    val wires = Source.fromFile(args(0)).getLines
    val wire1 = wires.next.split(",")
    val wire2 = wires.next.split(",")
    var grid = Map[List[Int], Set[String]]()
    println(shortestDistance(plotCoords(plotCoords(grid,wire1,"l1",List(0,0)),wire2,"l2",List(0,0))))
  }

  def plotCoords(grid: Map[List[Int], Set[String]], wire: Array[String], number: String, start: List[Int]): Map[List[Int], Set[String]] = {
    if (wire.length > 0)  {
      val end = start.zip(parseMove(wire(0))).map{case (a,b) => a+b}
      List(start,end) match {
        case List(List(x1,y1),List(x2,y2)) if x1 == x2 => drawY(grid,y1,y2,x1,number)
        case List(List(x1,y1),List(x2,y2)) if y1 == y2 => drawX(grid,x1,x2,y1,number)
      }
      return plotCoords(grid, wire.slice(1,wire.length), number, end)
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

  def drawX(grid: Map[List[Int], Set[String]], start: Int, end: Int, linePos: Int, number: String) = {
        val range = (start, end) match  {
          case (x1,x2) if x1 < x2 => (start to end)
          case (x1,x2) if x1 > x2 => (end to start)
          case _ => (start to end)
        }
        range.toList.slice(1,range.length-1).map{x => 
          if (grid.isDefinedAt(List(x,linePos))) {
            grid(List(x,linePos)) += number 
          }
          else  {
            grid(List(x,linePos)) = Set(number)
          }
        }
  }

  def drawY(grid: Map[List[Int], Set[String]], start: Int, end: Int, linePos: Int, number: String) = {
        val range = (start, end) match  {
          case (y1,y2) if y1 < y2 => (start to end)
          case (y1,y2) if y1 > y2 => (end to start)
          case _ => (start to end)
        }
        range.toList.slice(1,range.length-1).map{y => 
          if (grid.isDefinedAt(List(linePos,y))) {
            grid(List(linePos,y)) += number 
          }
          else  {
            grid(List(linePos,y)) = Set(number)
          }
        }
  }

  def shortestDistance(grid: Map[List[Int], Set[String]]): Int =  {
    val keys = grid.keys.filter(grid(_).size > 1)
    val absolute_keys = keys.map(_.map(math.abs(_)))
    return absolute_keys.map(_.sum).min
  }
}

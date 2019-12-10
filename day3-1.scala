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

  def plotCoords(grid: Map[List[Int], Int], wire: Array[String], pos: List[Int]): Map[List[Int], Int] = {
    if (wire.length > 0)  {
      val nextCoord = pos.zip(parseMove(wire(0))).map{case (a,b) => a+b}

      if (pos(0) == nextCoord(0)) {
        val start = math.min(pos(1), nextCoord(1))
        val end = math.max(pos(1), nextCoord(1))
        return plotCoords(draw(grid, start, end, pos(0), "x"), wire.slice(1,wire.length), nextCoord) 
      }
      else { //abusing the fact that I know in the context one of these will always be satisfied in the intended way - scala forces a catch all case for returning
        val start = math.min(pos(0), nextCoord(0))
        val end = math.max(pos(0), nextCoord(0))
        return plotCoords(draw(grid, start, end, pos(1), "y"), wire.slice(1,wire.length), nextCoord) 
      }
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

  def draw(grid: Map[List[Int], Int], start: Int, end: Int, linePos: Int, axis: String): Map[List[Int], Int] =  {
    if (start <= end)  {
      if (axis == "x")  {
        grid(List(linePos,start)) += 1 
      }
      if (axis == "y")  {
        grid(List(start,linePos)) += 1
      }
      return draw(grid, start+1, end, linePos, axis)
    }
    else  {
    if (axis == "x")  { //such a dirty hack but fuck it man I wanna move onto the next one
      grid(List(linePos,end)) -= 1 
    }
    if (axis == "y")  {
      grid(List(end,linePos)) -= 1
    }
      return grid
    }
  }

  def shortestDistance(grid: Map[List[Int], Int]): Int =  {
    grid(List(0,0)) = 0 //also a dirty hack
    return grid.keys.filter(grid(_) == 2).map((x,y) => math.abs(x) + math.abs(y)).min
  }
}

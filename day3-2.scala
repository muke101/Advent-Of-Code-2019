import scala.io.Source
import scala.math
import scala.collection.mutable.{Map,Set}

object Main {
  def main(args: Array[String]): Unit = {
    val wires = Source.fromFile(args(0)).getLines
    val wire1 = wires.next.split(",")
    val wire2 = wires.next.split(",")
    var grid = Map[(Int,Int), Set[String]]()
    val w1coords = plotCoords(List(List(),List()), wire1, (0,0))
    drawY(grid, w1coords(0), "w1")
    drawX(grid, w1coords(1), "w1")
    val w2coords = plotCoords(List(List(),List()), wire2, (0,0))
    drawY(grid, w2coords(0), "w2")
    drawX(grid, w2coords(1), "w2")
  }

  def plotCoords(coords: List[List[List[Int]]],  wire: Array[String], start: (Int,Int)): List[List[List[Int]]] = {
    if (wire.length > 0)  {
      val end = start.zip(parseMove(wire(0))).map{case (a,b) => a+b}
      (start,end) match {
        case ((x1,y1),(x2,y2)) if x1 == x2 => coords(0) += (y1,y2,x1) //first sublist for vertical lines, second for horizontal lines
        case ((x1,y1),(x2,y2)) if y1 == y2 => coords(1) += (x1,x2,y1)
      }
      return plotCoords(coords, wire.slice(1,wire.length), end)
    }
    else  {
      return coords
    }
  }

  def parseMove(move: String): (Int,Int) =  {
    val direction = move(0)
    val distance = move.slice(1,move.length).toInt

    return direction match  {
      case 'R' => (distance, 0)
      case 'L' => (-distance, 0)
      case 'U' => (0, distance)
      case 'D' => (0, -distance)
    }
  }

  def range(start: Int, end: Int): List[Int] = (start, end) match  {
          case (x1,x2) if x1 < x2 => (start to end).toList
          case (x1,x2) if x1 > x2 => (end to start).toList
          case _ => (start to end).toList
  }
  
  def drawX(grid: Map[(Int,Int), Set[String]], coords: List[List[Int]], number: String): Unit = {
    if (coords.length > 0)  {
      val point = coords(0)
      val start = point(0)
      val end = point(1)
      val linePos = point(2)
      range(start,end).slice(1,range.length-1).map{x =>
        if (grid.isDefinedAt((x,linePos))) {
            grid((x,linePos)) += number 
          }
          else  {
            grid((x,linePos)) = Set(number)
          }
      }
      drawX(grid, coords.slice(1,coords.length), number)
    }
  }

  def drawY(grid: Map[(Int,Int), Set[String]], coords: List[List[Int]], number: String): Unit = {
    if (coords.length > 0)  {
      val point = coords(0)
      val start = point(0)
      val end = point(1)
      val linePos = point(2)
      range(start,end).slice(1,range.length-1).map{y =>
        if (grid.isDefinedAt((linePos,y))) {
            grid((linePos,y)) += number 
          }
          else  {
            grid((linePos,y)) = Set(number)
          }
      }
      drawY(grid, coords.slice(1,coords.length), number)
    }
  }

  //def shortestDistance(grid: Map[(Int,Int), Set[String]]): Int =  {
  //  val keys = grid.keys.filter(grid(_).size > 1)
  //  val absolute_keys = keys.map(_.map(math.abs(_)))
  //  return absolute_keys.map(_.sum).min
  //}
}

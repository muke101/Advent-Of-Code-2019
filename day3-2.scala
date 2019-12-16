import scala.io.Source
import scala.math
import scala.collection.mutable.{Map,Set}

object Main {
  def main(args: Array[String]): Unit = {
    val wires = Source.fromFile(args(0)).getLines
    val wire1 = wires.next.split(",")
    val wire2 = wires.next.split(",")
    var grid = Map[(Int,Int), Set[String]]()
    val w1coords = plotCoords((List(),List()), wire1, (0,0))
    drawY(grid, w1coords._1, "w1")
    drawX(grid, w1coords._2, "w1")
    val w2coords = plotCoords((List(),List()), wire2, (0,0))
    drawY(grid, w2coords._1, "w2")
    drawX(grid, w2coords._2, "w2")
  }

  def plotCoords(coords: (List[List[Int]],List[List[Int]]),  wire: Array[String], start: (Int,Int)): (List[List[Int]],List[List[Int]]) = {
    if (wire.length > 0)  {
      val end = start.zip(parseMove(wire(0))).map{case (a,b) => a+b}
      (start,end) match {
        case ((x1,y1),(x2,y2)) if x1 == x2 => coords._1 += (y1,y2,x1) //first sublist for vertical lines, second for horizontal lines
        case ((x1,y1),(x2,y2)) if y1 == y2 => coords._2 += (x1,x2,y1)
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

  def line(start: Int, end: Int): List[Int] = (start, end) match  {
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
      val range = line(start,end)
      range.slice(1,range.length-1).map{x =>
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
      val range = line(start,end)
      range.slice(1,range.length-1).map{y =>
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

  def walkX(grid: Map[(Int,Int), Set[String]], coords: List[Int], destination: (Int,Int), distance: Int): ((Int,Int), Int) =  {
    val start = coords(0)
    val end = coords(1)
    val linePos = coords(2)
    var newDistance = distance
    line(start,end).map{x =>
      if ((x,linePos) == end) {
        return ((x,linePos),newDistance)
      }
      else  {
        newDistance+=1
      }
    return ((end,linePos),newDistance)
    }
  }

  def walkY(grid: Map[(Int,Int), Set[String]], coords: List[Int], destination: (Int,Int), distance: Int): ((Int,Int), Int) =  {
    val start = coords(0)
    val end = coords(1)
    val linePos = coords(2)
    var newDistance = distance
    line(start,end).map{y =>
      if ((linePos,y) == end) {
        return ((linePos,y),newDistance)
      }
      else  {
        newDistance+=1
      }
    return ((linePos,end),newDistance)
    }
  }



  def walk(grid: Map[(Int,Int), Set[String]], moves: List[String], start: (Int,Int), destination: (Int,Int), distance: Int): Int = {
    val result = plotCoords((List(),List()),Array(moves(0)),start) match {
       case (ys,xs) if ys.length == 0 => walkX(grid,xs(0),destination,distance)
       case (ys,xs) if xs.length == 0 => walkY(grid,ys(0),destination,distance)
    }
    val end = result._1
    val newDistance = result._2
    if (end == destination) {
       return newDistance
    }
    else {
       return walk(grid, moves.slice(1,moves.length), end, destination, newDistance)
    }
  }

  def shortestDistance(grid: Map[(Int,Int), Set[String]], wires: (List[String],List[String]), distances: List[Int], intersections: List[(Int,Int)]): Int =  {
    if (intersections.length > 0)  {
      val intersection = intersections(0)
      val w1Distance = walk(grid, wires._1, (0,0), intersection, 0)  
      val w2Distance = walk(grid, wires._2, (0,0), intersection, 0)
      distances += w1Distance+w2Distance
      return shortestDistance(grid, wires, distances, intersections.slice(1,intersections.length))
    }
    else  {
      return distances.min
    }
  }
}

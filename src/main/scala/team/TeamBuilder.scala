package team

import scala.collection.mutable.ListBuffer

class TeamBuilder {
  
  

  def specialLocations(paths: Array[String]): Array[Int] = {
    
    
    
    def canReachAllOther(p: Array[String], total: Int): Int = {

//      def analizePath(path: String, ind: Int, cur: Int, origin: Int) {
//        if (ind < path.length) {
//          if (cur == origin && ind == origin) access(origin)(ind) = 1
//          else { 
//            access(cur)(ind) = path(ind)
//            if (path(ind) == 1) analizePath(p(ind), 0, ind, origin)
//          }
//          analizePath(path, ind + 1, cur, origin)
//        }
//      }
//      for ( i <- 0 until p.length ) {
//       analizePath(p(i), 0, i, i)
//      }
      0
    }

    def reachable(ps: Array[String]): Int = {
      val reach = Array.ofDim[Int](paths.length, paths.length)
      val route = ListBuffer[(Int, Int)]()

      def trace(target: Int, origin: Int, current: Int, ind: Int) {
        if (origin < ps.length && current < ps.length) {
          if (target != origin && target != current) {
            if (ind < ps.length) {
              if (origin == current) {
                if (ps(origin)(ind).asDigit == 1) {
                  if (ind == target) {
                    reach(target)(origin) = 1
                    route.clear
                    route += ((origin + 1, 0))
                    trace(target, origin + 1, origin + 1, 0)
                  } else {
                    route.find(a => a._1 == ind) match {
                      case Some(y) => 
                        if (y._2 < ps.length) trace(target, origin, ind, 0)
                        else //HERE
                      case None => 
                    }
                    if (!route.contains(ind)) {
                      reach(target)(origin) = 0
                      route += ind
                      trace(target, origin, ind, 0, ind)
                    } else {
                      route.clear
                      route += origin + 1
                      trace(target, origin + 1, origin + 1, 0, 0)
                    }
                  }
                } else {
                  reach(target)(origin) = 0
                  trace(target, origin, current, ind + 1, ind + 1)
                }
              } else {
                if (ps(current)(ind).asDigit == 1) {
                  if (ind == target) {
                    reach(target)(origin) = 1
                    route.clear
                    route += origin + 1
                    trace(target, origin + 1, origin + 1, 0, 0)
                  } else {
                    if (!route.contains(ind)) {
                      reach(target)(origin) = 0
                      trace(target, origin, ind, 0, ind)
                    } 
//                    else if (oInd < ps.length)
//                      trace(target, origin, route.takeRight(2).head, oInd + 1, oInd + 1)
                    else
                      trace(target, origin, current, ind + 1, ind + 1)
                  }
                } else {
                  reach(target)(origin) = 0
                  trace(target, origin, current, ind + 1, ind + 1)
                }
              }
            } else {
              route.clear
              route += origin + 1
              trace(target, origin + 1, origin + 1, 0, 0)
            }

          } else {
            if (target == origin) {
              reach(target)(origin) = 1
              route.clear
              route += origin + 1
              trace(target, origin + 1, origin + 1, 0, 0)
            } else {
              trace(target, origin, current + 1, 0, 0)
            }
          }
        } //end
      }

      for (i <- 0 until ps.length) {
        route.clear
        route += 0
        trace(i, 0, 0, 0, 0)
      }

      reach.map(a => if (a.sum == ps.length) 1 else 0).sum

    }
      
      
    
    Array(canReachAllOther(paths, 0),reachable(paths)) 
  }
  
}
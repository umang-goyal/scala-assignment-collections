package com.knoldus

class MapCollections {
  def sumValueofSimilarKey(map: Map[String, Int], str:String):Int= {
    map.foldLeft(0) {
      (a: Int, element: (String, Int)) => if (element._1.contains(str)) {
        a + element._2
      } else a + 0
    }
  }

  def last(list: List[Int]):Any = {
    list match {
      case Nil => "empty list"
      case _ =>  list.foldLeft(0){(a,b)=>b}
    }

  }
}

object MapCollections extends App{
  val obj = new MapCollections
  val map = Map("suresh"->4, "ramesh"-> 3, "suraj"-> 2)
  println(obj.sumValueofSimilarKey(map, "su"))
  println(obj.last(List(1,2,3,4,5)))
}
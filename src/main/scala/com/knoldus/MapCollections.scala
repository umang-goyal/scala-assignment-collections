package com.knoldus

class MapCollections {
  def sumValueofSimilarKey(map: Map[String, Int], str:String):Int= {
    map.foldLeft(0) {
      (a: Int, element: (String, Int)) => if (element._1.contains(str)) {
        a + element._2
      } else a + 0
    }
  }

  def changeMap (map: Map[Int, List[String]]): Map[Int, List[String]]= {

    val newMap = Map[Int, List[String]]()

    map.foldLeft(newMap){
      (a:Map[Int, List[String]], elem: (Int, List[String]))=> if (elem._1%2==0){
        a++ Map(elem._1-> elem._2.map(x=> x.head+"even"))
      }
      else {
        a++ Map(elem._1-> elem._2.map(x=> x.head+"odd"))
      }
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
  val map2 = Map(1 -> List("Sunil", "Laxmi"),
    2 -> List("Bhavya", "Sangeeta"),
    3 -> List("Arun", "Sushmita"),
    4 -> List("Jamwant")
  )
  println(obj.changeMap(map2))
}

package com.knoldus

class Collections {

  def kth(list: List[Int], index:Int):Any= {
    @scala.annotation.tailrec
    def inner(list: List[Int], index: Int, count: Int): Any={
      list match {
        case Nil => "empty list"
        case a :: rest if count == index => a
        case a :: rest if count != index => inner(rest, index, count+1)
      }
    }
    inner(list, index, 0)
  }



  def dropLast(list: List[Int]):List[Int]= {
    val index = list.size-1
    @scala.annotation.tailrec
    def inner(list: List[Int], index: Int, count: Int, newList:List[Int]): List[Int]={
      list match {
        case Nil => list
        case a :: rest if count == index => newList
        case a :: rest if count != index => inner(rest, index, count+1, newList:+ a)
      }
    }
    inner(list, index, 0, List())
  }


  @scala.annotation.tailrec
  final def isPalindrome(list:List[Int]):Boolean= list match{
    case a::Nil  => false
    case Nil => false
    case a::b::Nil if a==b =>true
    case a::b::c::Nil if a==c =>true
    case a::rest if a==rest.last && rest.nonEmpty => isPalindrome(dropLast(rest))
  }



  def reverse(list: List[Int]): Any ={
    @scala.annotation.tailrec
    def inner(list: List[Int], reversedList: List[Int]):Any = {
      list match {
        case Nil => "empty list"
        case a::Nil => a::reversedList
        case a::rest => inner(rest, a::reversedList)
      }
    }
    inner(list,List())
  }




  @scala.annotation.tailrec
  final def firstEven(list:List[Int]): Any ={
    list match {
      case Nil => "empty list"
      case a::Nil if a%2==0 => a
      case a::Nil if a%2!=0 => "even number not pesent"
      case a::rest if a%2==0 => a
      case a::rest if a%2!=0 => firstEven(rest)
    }
  }



  def compress(list: List[Int]): Any = {
    @scala.annotation.tailrec
    def innerCompress(list: List[Int], eliminatedList: List[Int]): Any = {
      list match {
        case Nil => "list is empty"
        case a :: b :: rest => if(a == b) {
          innerCompress(b :: rest, eliminatedList)
        }
        else {
          innerCompress(b :: rest, eliminatedList :+ a)
        }
        case a :: Nil => eliminatedList :+ a
      }
    }
    innerCompress(list, List())
  }




  def duplicate(list: List[Int]): Any = {
    @scala.annotation.tailrec
    def inner(list: List[Int], dupicatedList: List[Int]): Any = {
      list match {
        case Nil => "empty list"
        case a::Nil => dupicatedList::: List(a,a)
        case a::rest => inner(rest, dupicatedList::: List(a,a))
      }
    }

    inner(list, List())
  }




  def drop(list: List[Int], elem: Int): Any ={
    @scala.annotation.tailrec
    def inner(list: List[Int], elem:Int, newList: List[Int]):Any= {
      list match {
        case Nil => "empty list"
        case a :: Nil if a != elem => newList:+a
        case a :: Nil if a == elem => newList
        case a :: rest if a == elem => inner(rest, elem, newList ::: rest)
        case a :: rest if a != elem => inner(rest, elem, newList:+a)
      }
    }
    inner (list, elem, List())
  }

}

object Collections extends App{
  val obj = new Collections
  println(obj.kth(List(1,2,3,4,5),3))
  val list1= List(1,2,3,8,3,2,1)
  println(obj.isPalindrome(list1))
  val list2 = List(1, 2, 3, 1, 4, 5)
  println(obj.reverse(list2))
  val list3 = List(1, 2, 3, 1, 4, 5,6)
  println(obj.firstEven(list3))
  val list4 = List(1, 1, 1, 1, 2, 3, 3, 1, 1, 4, 5, 5, 5, 5)
  println(obj.compress(list4))
    println(obj.duplicate(List(1, 2, 3, 3, 4)))
    println(obj.drop(List(1,2,3,4,5,6,7,8,9,10,11),3))

}

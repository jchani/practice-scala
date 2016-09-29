package exercises

import sys.error
import scala.collection.mutable.ListBuffer

/**
 * The goal of these exercises is to get familiar with collections in Scala.
 * We'll use immutable collections, which means that--once created--the value
 * of a collection can't change. If we want to modify a collection, we'll likely
 * call a method that results in a new collection. We often operate over
 * collections in a functional way (e.g., as in Racket): using recursion and 
 * higher-order functions instead of a loop. 
 * 
 * Here are some good resources for learning about Scala collections:
 * 
 * A series of shortish articles about Scala collections:
 * 	http://docs.scala-lang.org/overviews/collections/introduction
 * 
 *  The List API:
 * 		http://www.scala-lang.org/api/current/#scala.collection.immutable.List
 *  
 * The exercises in this file are taken from Scala Labs:
 * 	https://github.com/scala-labs/scala-labs/tree/master/labs/src/main/scala/org/scalalabs/basic/lab02
 */
object Collections {

  /**
   * Get the first element in the list. 
   * 
   * Hint: there is a built-in function for this you can use.
   * 
   */
  def firstElementInList[T](l: List[T]): T = {
    l(0)
  }

  /**
   * Get the sum of all the elements in the list, e.g. sumOfList(List(1,2,3)) = 6.
   */
  def sumOfList(l: List[Int]): Int = l match{
    case Nil => 0
    case l::rest => l + sumOfList(rest)
  }

  /**
   * Get the last element in the list, e.g. lastElementInList(List(1,2,3)) = 3.
   * 
   * Hint: this can be achieved in multiple ways:
   *  - built in
   *  - via a pattern match
   *  - by using a foldLeft function
   *  - ... etc
   */
  def lastElementInList[T](l: List[T]): T = {
    l(l.length-1)
  }

   /**
   * Get the nth element in the list, e.g. nthElementInList(3, List(1,2,3,4)) = 3.
   * 
   * Hint: this can be achieved in multiple ways:
   *  - built in
   *  - via a pattern match
   *  - custom made (for instance, it can be done in a fun way by using the 
   *    zipWithIndex function, that is available on a List)
   *  - ... etc
   */
  def nthElementInList[T](n: Int, l: List[T]): T = {
    l(n)
  }

  /**
   * Concatenate two lists into one, e.g. 
   * concatLists(List(1,2,3), List(4,5,6)) = List(1,2,3,4,5,6)
   * 
   * Hint: this can be achieved in multiple ways:
   *  - built in
   *  - via a pattern match
   *  - custom made
   *  - ... etc 
   */
  def concatLists[T](l1: List[T], l2: List[T]): List[T] = {
    List.concat(l1, l2)
  }

  /**
   * Sort a list on the natural ordering, so sortList(3,1,2) = List(1,2,3).
   * 
   * Hint: this can be achieved in multiple ways:
   * - built in
   * - via a foldLeft method (a bit complex, but fun)
   * - ... whichever way you like 
   * 
   */
  def sortList[T <% Ordered[T]](list: List[T]): List[T] = {
    list.sorted
  }

  /**
   * Check whether a given element in a list exists, 
   * i.e. elementExists(List("a", "b", "c"), "b") = true
   * 
   * Again, easy to implement using built-in functionality, but also possible 
   * to implement in your own free-style way.
   */
  def elementExists[T](l: List[T], e: T): Boolean = {
    l.contains(e)
  }

  /**
   * Get all odd elements in the list, i.e. 
   * oddElements(List(1,2,3,4,5)) = List(1,3,5)
   * 
   * As always, use either build-in functions, or roll your own way via a
   * pattern match or some other method.
   */
  def oddElements(iList: List[Int]): List[Int] = iList match{
    case odd::even::rest => odd::oddElements(rest)
    case odd::Nil => List(odd)
    case Nil => List()
  }

  /**
   * Returns a list of lists, containing all final segments of the argument 
   * list, longest first.
   * 
   * For example: tails(List(1,2,3,4)) = List(List(1,2,3,4), List(2,3,4), List(3,4), List(4), List())
   * 
   * Inspired by Haskell's tails function: 
   * 	http://www.zvon.org/other/haskell/Outputlist/tails_f.html
   * 
   * Implement it whatever way suites you best. Hint: it can be done in a 
   * neat way using recursion. 
   */
  def tails[T](l: List[T]): List[List[T]] = l match {
    case Nil => List(List())
    case _ :: t => l::tails(t)
  }
  
  /**
   * Find the maximum element in a list, e.g. maxElementInList(List(1,9,3,5)) == 9
   * 
   * As usual, various ways exist: pattern matching, folding, ...
   */
  def maxElementInList(l: List[Int]): Int = {
    l.max
  }

  /**
   * Calculate the sum of the equally position elements
   * of the two list
   */
  def sumOfTwo(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match{
    case (e1::rest1, e2::rest2) => (e1+e2)::sumOfTwo(rest1, rest2)
    case (e1::rest1, Nil) => l1
    case (Nil, e2::rest2) => l2
    case(Nil, Nil) => List()
  }

  /**
   *  For this exercise preferably make use of the sumOfTwo
   * method above
   */
  def sumOfMany(l: List[Int]*): List[Int] = {l.fold(List())(sumOfTwo) //using fold on a list of lists (therefore, startval must be the type List())
  }

  case class Person(age: Int, firstName: String, lastName: String)

  /**
   * The following method is implemented in the most inelegant way we could think of.
   * The idea is to rewrite the method into more functional style. 
   */
  def separateTheYoungFromTheOld(persons: List[Person]): List[List[String]] = {
    var youngins: ListBuffer[Person] = new ListBuffer[Person]()
    var elders: ListBuffer[Person] = new ListBuffer[Person]()
    var validYoungNames: ListBuffer[String] = new ListBuffer[String]()
    var validOldNames: ListBuffer[String] = new ListBuffer[String]()

    for (person <- persons) {
        if (person.age < 18) {
          youngins += person
        } else {
          elders += person
        }
    }

    var sortedYoung = youngins.toList.sortBy(_.age)
    var sortedOld = elders.toList.sortBy(_.age)

    for (young <- sortedYoung) {
      validYoungNames += young.firstName
    }
    for (old <- sortedOld) {
      validOldNames += old.firstName
    }
    
    List(validYoungNames.toList, validOldNames.toList)
  }
}



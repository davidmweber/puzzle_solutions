package co.horn.puzzle

import scala.util.Random

object PuzzleTime {
  
  type ??? = Nothing

  /**
   * Problem 01
   * P01 (*) Find the last element of a list.
   * Example:
   *  scala> last(List(1, 1, 2, 3, 5, 8))
   *  res0: Int = 8
   */
  def last[A](l: List[A]): A = l match {
    case head :: Nil ⇒ head
    case _ :: tail ⇒ last(tail)
    case Nil ⇒ throw new NoSuchElementException("Empty list cannot have a final element")
  }

  /**
   * Problem 02
   * Find the last but one element of a list.
   * Example:
   *  scala> penultimate(List(1, 1, 2, 3, 5, 8))
   *  res0: Int = 5
   */
  def penultimate[A](l: List[A]): A = l match {
    case x :: _ :: Nil => x
    case head :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException("Penultimate requires a list to have at least 2 elements")
  }

  /**
   * Problem 03
   * Find the K'th element. By convention, the first element in the list is element 0.
   * Example:
   *  scala> kth_element (2, List(1, 1, 2, 3, 5, 8))
   *  res0: Int = 2
   */
  def kth_element[A](k: Int, l: List[A]): A = l match {
    case head :: _  if  k == 0 => head
    case head :: tail => kth_element(k-1, tail)
    case _ => throw new NoSuchElementException("List does not have k elements")
  }

  /**
   * Problem 04
   * Find the number of elements of a list.
   * Example:
   *  scala> length(List(1, 1, 2, 3, 5, 8))
   *  res0: Int = 6
   */
   def count_elems[A](l: List[A]): Int = {
     l.foldLeft(0)((count,_) ⇒ count + 1)
  }

  /**
   * Problem 05
   * Reverse a list.
   *   Example:
   *   scala> reverse(List(1, 1, 2, 3, 5, 8))
   * res0: List[Int] = List(8, 5, 3, 2, 1, 1)
   */
  def reverse[A](l: List[A]): List[A] =  l.foldLeft(List[A]())((n,v) ⇒ v :: n)
  /**
   * Problem 06
   * Find out whether a list is a palindrome.
   * Example:
   *   scala> isPalindrome(List(1, 2, 3, 2, 1))
   * res0: Boolean = true
   */
  def isPalindrome[A](l: List[A]): Boolean = l == reverse(l)

  /**
   * Problem 07
   * Flatten a nested list structure.
   *   Example:
   *   scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
   * res0: List[Any] = List(1, 1, 2, 3, 5, 8)
   */
  def flatten[_](l: List[_]): List[_] =  l match {
    case (head: List[_])::Nil => flatten(head)
    case (head: List[_])::tail => flatten(head):::flatten(tail)
    case head::Nil => List(head)
    case head::tail => head::flatten(tail)
    case Nil => Nil
  }
  
  /**
   * Problem 08
   * Eliminate consecutive duplicates of list elements.
   * If a list contains repeated elements they should be replaced with a
   * single copy of the element. The order of the elements should not be changed.
   *   Example:
   *   scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   *   res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
   */
  def compress[A](l: List[A]): List[A] = l match {
    case head :: Nil => List(head)
    case head :: tail => head ::compress(tail.dropWhile(_ == head))
    case Nil => Nil
  }

  /**
   * Problem 09
   * Pack consecutive duplicates of list elements into sublists.
   * If a list contains repeated elements they should be placed in separate sublists.
   *   Example:
   *   scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   *   res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
   */
  def pack[A](l : List[A]) : List[List[A]] = l match {
    case head :: Nil => List(List(head))
    case head :: tail =>
      def extractLike(head : A, list : List[A], oldList : List[A] ) : (List[A], List[A]) = oldList  match {
        case x :: t if x == head => extractLike(head, x :: list, t)
        case x => (list, x)
      }
      val (like, unlike) = extractLike(head, head :: Nil, tail)
      like :: pack(unlike)
    case Nil => Nil
  }

  /**
   * Problem 10
   * Run-length encoding of a list. Use the result of problem P09 to implement
   * the so-called run-length encoding data compression method. Consecutive
   * duplicates of elements are encoded as tuples (N, E) where N is the
   * number of duplicates of the element E.
   * Example:
   *   scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   *   res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   */
  def encode[A](l: List[A]): List[(Int,A)] = pack(l) map { e => (e.length, e.head) }

  /**
   * Problem 11
   * Modify the result of problem P10 in such a way that if an element has
   * no duplicates it is simply copied into the result list. Only
   * elements with duplicates are transferred as (N, E) terms.
   * Example:
   *   scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   *   res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
   */
  def encodeMod[A](l: List[A]): List[Any] = encode(l) map { t => if (t._1 == 1) t._2 else t }

  /**
   * Problem 12
   * Remove the Kth element from a list. Return the list and the removed
   * element in a Tuple. Elements are numbered from 0.
   * Example:
   *   scala> removeAt(1, List('a, 'b, 'c, 'd))
   *   res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
   */
  def removeAt[A](n: Int, l: List[A]): (List[A], A) = l.splitAt(n) match {
    case (Nil, _) if n < 0 => throw new NoSuchElementException("Index out of range")
    case (pre, e :: post)  => (pre ::: post, e)
    case (pre, Nil)        => throw new NoSuchElementException("Index out of range")
  }

  /**
   * Problem 13
   * Extract a given number of randomly selected elements from a list.
   * Example:
   *   scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
   *   res0: List[Symbol] = List('e, 'd, 'a)
   */
  def randomSelect[A](n: Int, l: List[A]): List[A] = {
    if (n <= 0) Nil
    else {
      val (rest, e) = removeAt(Random.nextInt(l.length), l)
      e :: randomSelect(n - 1, rest)
    }

  }

  /**
   * Problem 14
   * Generate a random permutation of the elements of a list.
   * Example:
   *   scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
   *   res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
   */
  def randomPermute1[A](ls: List[A]): List[A] = randomSelect(ls.length, ls)

}

package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      value <- arbitrary[Int]
      heap <- oneOf(const(empty), genHeap)
    } yield insert(value, heap))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert 2 elements into a heap") = forAll { (v1: Int, v2: Int) =>
    findMin(insert(v1, insert(v2, empty))) == (if (v1 < v2) v1 else v2)
  }

  property("insert then deleteMin to a empty heap") = forAll { (v: Int) =>
    isEmpty(deleteMin(insert(v, empty)))
  }

  def insertList(list: List[A], h: H): H = {
    list match {
      case Nil     => empty
      case x :: xs => insert(x, insertList(xs, h))
    }
  }
  property("correct min") = forAll { (l: List[A]) =>
    if (l.isEmpty) isEmpty(insertList(l, empty)) else findMin(insertList(l, empty)) == l.min
  }

  def heapSort(h: H): List[A] = {
    if (isEmpty(h))
      List()
    else
      findMin(h) :: heapSort(deleteMin(h))
  }

  property("heap sort") = forAll { (l: List[A]) =>
    val lSorted = heapSort(insertList(l, empty))
    if (l.isEmpty) lSorted.isEmpty else (l.sorted, lSorted).zipped.forall(ord.equiv(_, _))
  }

  def min(a: Int, b: Int) = if (a < b) a else b

  property("minimum of melding") = forAll { (h1: H, h2: H) =>
    val meldH = meld(h1, h2)
    if (isEmpty(h1))
      if (isEmpty(h2))
        isEmpty(meldH)
      else
        findMin(meldH) == findMin(h2)
    else if (isEmpty(h2))
      findMin(meldH) == findMin(h1)
    else
      findMin(meldH) == min(findMin(h1), findMin(h2))
  }

}

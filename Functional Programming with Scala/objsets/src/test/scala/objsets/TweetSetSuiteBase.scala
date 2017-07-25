package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuiteBase extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("union: base") {
    new TestSets {
      assert(size(set1.union(set1)) === 0)
    }
  }

  test("union: 1 vs empty") {
    new TestSets {
      assert(size(set2.union(set1)) === 1)
    }
  }

  test("union: empty vs 1") {
    new TestSets {
      assert(size(set1.union(set2)) === 1)
    }
  }
}

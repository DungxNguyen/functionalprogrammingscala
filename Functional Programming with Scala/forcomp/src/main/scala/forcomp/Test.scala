package forcomp

object Test {
  import Anagrams._

  def main(args: Array[String]){
    val list = List(1, 2, 6, 1)
    println((list foldLeft 0)((cummu, x) => x + cummu))
    
    val a = 'a'
    val list2 = List((a, 1), (a, 2), (a, 3))
    println(list2.groupBy(x => x._1).map(kv => (kv._1, kv._2.unzip._2)))
    

    val temp = dictionary.map(word => List((word,wordOccurrences(word))))
//    println(temp.take(10))
    
    println("Combination: " + combinations(List(('a', 2), ('b', 2))))
    
    println(sentenceAnagrams(List("i", "love", "you")))
  }
}
package objsets

object Test {
  
  def main(args: Array[String]): Unit = {
    println(List[Int](1, 2, 3, 4).reduceLeft(_ + _))

    println(List[Int](1, 2, 3, 4).foldLeft(1)(_ + _))

    println(List[Int](1, 2, 3, 4).forall(_.isValidInt))

  }
}
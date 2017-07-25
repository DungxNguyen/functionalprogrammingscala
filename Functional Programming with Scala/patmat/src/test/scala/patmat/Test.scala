package patmat

object Test {
  trait T1 {
    def method(x: Int): Int = x
  }  
  
  trait T2 {
    def method(x: Int): Int = x * x
  }
  
  abstract class T {
    def method(x: Int): Int = -x 
  }
  class TT extends T with T1 with T2 {
    override def method(x: Int): Int = super[T2].method(x)  
  }
  
  def main(args: Array[String]): Unit = println(new TT().method(2))
}
object worksheet{;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(29); 
  val a = 1;System.out.println("""a  : Int = """ + $show(a ));$skip(15); 
  
  var b = 2;System.out.println("""b  : Int = """ + $show(b ));$skip(17); 
  
  val c = 'a';System.out.println("""c  : Char = """ + $show(c ));$skip(9); val res$0 = 

  a + b;System.out.println("""res0: Int = """ + $show(res$0));$skip(17); 

  val d = a + b;System.out.println("""d  : Int = """ + $show(d ));$skip(10); val res$1 = 

  d == 4;System.out.println("""res1: Boolean = """ + $show(res$1))}

}

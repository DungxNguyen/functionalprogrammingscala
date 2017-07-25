package forcomp

object worksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(47); 
  val a = 1;System.out.println("""a  : Int = """ + $show(a ));$skip(15); 
  
  var b = 2;System.out.println("""b  : Int = """ + $show(b ));$skip(17); 
  
  val c = 'a';System.out.println("""c  : Char = """ + $show(c ))}

}

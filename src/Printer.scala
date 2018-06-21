

object Printer {
  def stringOfOperation(o:Operation): String= o match{
    case Plus() => "+"
    case Minus() => "-"
    case Times() => "*"
    case Slash() => "/"
    case Less() => "<"
    case Greater() => ">"
    case LessEq() => "<="
    case GreaterEq() => "=>"
    case Equal() => "="
    case And() => "&"
    case Or() => "|"
      
  } 
    
  def stringOfExp(e:Exp): String= e match{
    case  IntCst(i)=> String.valueOf(i)
    case  Var(v) => v
    case  Oper(o,e1,e2) => 
     "("+stringOfExp(e1)+" "+stringOfOperation(o)+" "+stringOfExp(e2)+")"
  }
  
  def indent(n:Int):String={
    if(n==0)
      ""
    else 
      " " + indent(n-1)
 
  }
  
  def stringOfComm(tab:Int,c:Comm): String= c match{
    case Null() => indent(tab) + "null" + ";\n"
    case Assign(v,e) =>indent(tab) + v + " := " + stringOfExp(e) + ";\n"
    case Seq(c1,c2) =>indent(tab) + stringOfComm(tab,c1) + stringOfComm(tab,c2)
    case While(e,c) =>indent(tab) + "while" + stringOfExp(e) + "loop\n" + stringOfComm(tab,c) + indent(tab) + "end loop" + ";\n"
    case IfThen(e,c) =>indent(tab) + "if" + stringOfExp(e) + "then\n" + stringOfComm(tab,c) + indent(tab) + "end if" + ";\n"
    case IfThenElse(e,c1,c2) =>indent(tab) + "if" + stringOfExp(e) + "then\n" + stringOfComm(tab,c1) + indent(tab) + "else" + stringOfComm(tab,c2) + indent(tab) + "end if" + ";\n"
    case CommRegion(c, l1, l2) =>"-- {"+ l1 + "-" + l2 + "}\n" + stringOfComm(tab,c) 
  }
  def main (args: Array[String]):Unit ={
    val c = Parser.parseFile("resources/test1.adb")
    print(stringOfComm(0,c))
    
  }
    
}
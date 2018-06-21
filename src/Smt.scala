import scala.collection.mutable.Map
import scala.collection.mutable.Queue
import scala.collection.mutable.Set

object Smt {
  
  def getAllVars(l: Exp): Set[String] = l match {
    case IntCst(i) => Set()
    case Var(v) => Set(v)
    case Oper(o, e1, e2) => getAllVars(e1) ++ getAllVars(e2)
  }
  
  def getAllVars(l: List[(String, Exp)]): Set[String] = {
    var set: Set[String] = Set()
    for ((v, e) <- l) {
      if (v != "true" && v != "false")
        set = set ++ Set(v)
      set = set ++ getAllVars(e)
    }
    set
  }
  
  def smtStringOfExp(e: Exp): String = e match {
    case IntCst(i) => i.toString()
    case Var(v) => v
    case Oper(o, e1, e2) => "(" + Printer.stringOfOperation(o) + " " + smtStringOfExp(e1) + " " + smtStringOfExp(e2) + ")"
  }
  
  def smtDisplayDeclarations(l: Set[String]): Unit = {
    println("; Variable declarations")
    for (str <- l) 
      println("(declare-fun " + str + " () Int)")
  }
  
  def smtDisplayConstraints(l: List[(String, Exp)]): Unit = {
    println("; Constraint")
    for ((v, e) <- l) {
      println("(assert (= " + v + " " + smtStringOfExp(e) + "))")
    }
  }
  
  def smtDisplayAll(l: List[(String, Exp)]): Unit = {
    val vars = getAllVars(l)    
    println;
    smtDisplayDeclarations(vars)
    println;
    smtDisplayConstraints(l)
    println;
    println("; Solve")
    println("(check-sat)")
    println("(get-model)")
  }
  
  def main(args: Array[String]): Unit = {
    val c = Parser.parseFile("resources/test1.adb")
    val m = Map("inf" -> 2, "sup" -> 15)
    println(m)
    val p = Interpret.traceComm(c, m)
    println(p)
    println(m)
    
    val ds = Queue(true, true, true, true, true, true, false)
    val versions: Map[String, Int] = Map()
    println(ds)
    val l = Interpret.symbolic(c, ds, versions)
    //println(l)
    //println(l.map({ case(v,e) => v = " = " + Printer.stringOfExp(e) }))
    smtDisplayAll(l)
  }
}
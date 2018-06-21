import scala.collection.mutable.Map
import scala.collection.mutable.Queue


object Interpret {
  
  def evalExp(e: Exp, m: Map[String, Int]): Int = e match {
    case IntCst(i) => i
    case Var(s) => m.getOrElse(s, 0)
    case Oper(o, e1, e2) => o match {
      case Plus() => evalExp(e1, m) + evalExp(e2, m)
      case Minus() => evalExp(e1, m) - evalExp(e2, m)
      case Times() => evalExp(e1, m) * evalExp(e2, m)
      case Slash() => evalExp(e1, m) / evalExp(e2, m)
      case Less() => if (evalExp(e1, m) < evalExp(e2, m)) 1 else 0
      case Greater() => if (evalExp(e1, m) > evalExp(e2, m)) 1 else 0
      case LessEq() => if (evalExp(e1, m) <= evalExp(e2, m)) 1 else 0
      case Equal() => if (evalExp(e1, m) == evalExp(e2, m)) 1 else 0
      case And() => if (evalExp(e1, m) == 1 && evalExp(e2, m) == 1) 1 else 0
      case Or() => if (evalExp(e1, m) == 1 || evalExp(e2, m) == 1) 1 else 0
    }
  }
  
  def traceComm(c: Comm, m: Map[String, Int]): List[Boolean] = c match {
    case Null() => List()
    
    case Assign(v, e) => {      
      val i = evalExp(e, m)
      m.put(v, i); List()
    }
    
    case Seq(c1, c2) => {
      val p1 = traceComm(c1, m)
      val p2 = traceComm(c2, m)
      p1 ++ p2
    }
    
    case While(e, c) => {
      var l: List[Boolean] = List() 
      while(evalExp(e, m) == 1) {
        l = l ++ List(true)
        l = l ++ traceComm(c, m)
      }
      l :+ false
    }
    
    case IfThen(e, c) => {
      if (evalExp(e, m) == 1)
        true :: traceComm(c, m)
      else 
        List(false)
    }
    
    case IfThenElse(e, c1, c2) => {
      if (evalExp(e, m) == 1) 
        true :: traceComm(c, m)
      else
        false :: traceComm(c, m)
    }
    
    case CommRegion(c, i1, i2) => {
      traceComm(c, m)
    }
  }
  
  def next(ds: Queue[Boolean]): Boolean = {
    if (ds.isEmpty) false
    else ds.dequeue()
  }
  
  def numExp(e: Exp, versions: Map[String, Int]): Exp = e match {
    case IntCst(i) => IntCst(i)
    case Var(v) => 
      val k = versions.getOrElse(v, 0)
      if (k > 0) Var(v + "_" + k)
      else Var(v)
    case Oper(o, e1, e2) => Oper(o, numExp(e1, versions), numExp(e2, versions))
  }
  
  def symbolic(c: Comm, ds: Queue[Boolean], versions: Map[String, Int]): List[(String, Exp)] = c match {
    case Null() => List()
    
    case Pragma(v, e) => List((v, numExp(e, versions)))
    
    case Assign(v, e) => {
      val k: Int = 1 + versions.getOrElse(v, 0)
      val l = List((v + "_" + k, numExp(e, versions)))
      versions.put(v, k)
      l
    }
    
    case Seq(c1, c2) => {
      val p1 = symbolic(c1, ds, versions)
      val p2 = symbolic(c2, ds, versions)
      p1 ++ p2
    }
    
    case While(e, comm) => {
      var l: List[(String, Exp)] = List() 
      while(next(ds)) {
        l = l ++ List(("true", numExp(e, versions)))
        l = l ++ symbolic(comm, ds, versions)
      }
      l ++ List(("false", numExp(e, versions)))
    }
    
    case IfThen(e, c) => {
      if (next(ds))
        ("true", numExp(e, versions)) :: symbolic(c, ds, versions)
      else 
        List(("false", numExp(e, versions)))
    }
    
    case IfThenElse(e, c1, c2) => {
      if (next(ds)) 
        ("true", numExp(e, versions)) :: symbolic(c1, ds, versions)
      else
        ("false", numExp(e, versions)) :: symbolic(c2, ds, versions)
    }
    
    case CommRegion(c, i1, i2) => {
      symbolic(c, ds, versions)
    }
  }
}
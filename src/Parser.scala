import java.io.Reader
import java.io.FileReader
import java.io.IOException
import java.util.NoSuchElementException

import Lexer._
import Util._

abstract class Typ
case class Integer () extends Typ
case class Bool () extends Typ

abstract class Operation
case class Plus () extends Operation
case class Minus () extends Operation
case class Times () extends Operation
case class Slash () extends Operation
case class Less () extends Operation
case class Greater () extends Operation
case class LessEq () extends Operation
case class GreaterEq () extends Operation
case class Equal () extends Operation
case class And () extends Operation
case class Or () extends Operation

abstract class Exp
case class IntCst (field1: Int) extends Exp
case class Var (field1: String) extends Exp
case class Oper (field1: Operation, field2: Exp, field3: Exp) extends Exp

abstract class Comm
case class Null () extends Comm
case class Assign (field1: String, field2: Exp) extends Comm
case class Seq (field1: Comm, field2: Comm) extends Comm
case class While (field1: Exp, field2: Comm) extends Comm
case class IfThen (field1: Exp, field2: Comm) extends Comm
case class IfThenElse (field1: Exp, field2: Comm, field3: Comm) extends Comm
case class CommRegion (field1: Comm, field2: Int, field3: Int) extends Comm

object Parser {

  
  def getLine(l: List[(Token, Int)]) : Int  = {
    l match {
      case Nil => 
        throw new Exception
      case (h :: _) => 
        snd(h)
    }
  }
  
  def getLastLine(l: List[(Token, Int)]) : Int  = {
    l match {
      case Nil => 
        throw new Exception
      case (h1 :: l1) => 
        l1 match {
          case Nil => 
            snd(h1)
          case (_ :: _) => 
            getLastLine(l1)
        }
    }
  }
  
  def make_seq(l: List[Comm]) : Comm  = {
    l match {
      case Nil => 
        Null()
      case (i :: l2) => 
        l2 match {
          case Nil => 
            i
          case (_ :: _) => 
            Seq(i, make_seq(l2))
        }
    }
  }
  def parse: ((Token, List[(Token, Int)])) => List[(Token, Int)] = {
    (pair: (Token, List[(Token, Int)])) => {
      val (t, s) = pair
      s match {
        case Nil => 
          throw new Exception("Unexpected EOF")
        case (h :: s2) => 
          val (tok, line) = h
          if (tok == t) {
            s2
          } else {
            throw new Exception(("Syntax error line " + (String.valueOf(line) + (":" + (" expecting '" + (token_to_string(t) + ("'" + (" found '" + (token_to_string(tok) + "'")))))))))
          }
      }
    }
  }
  def parse_decision: (List[(Token, Int)]) => (Exp, List[(Token, Int)]) = {
    (s: List[(Token, Int)]) => {
      def result: (Exp, List[(Token, Int)]) = {
        val (e1, s1) = parse_nested_decision(s)
        s1 match {
          case Nil => 
            throw new Exception("Unexpected EOF")
          case (h :: s2) => 
            val (tok, _) = h
            tok match {
              case T_AND() => 
                val (e2, s3) = parse_decision(s2)
                (Oper(And(), e1, e2), s3)
              case T_OR() => 
                val (e2, s3) = parse_decision(s2)
                (Oper(Or(), e1, e2), s3)
              case _ => 
                (e1, s1)
              }
        }
      }
      result
    }
  }
  def parse_nested_decision: (List[(Token, Int)]) => (Exp, List[(Token, Int)]) = {
    (s: List[(Token, Int)]) => {
      def result: (Exp, List[(Token, Int)]) = {
        s match {
          case Nil => 
            throw new Exception("Unexpected EOF")
          case (h :: s1) => 
            val (tok, _) = h
            tok match {
              case T_LPAR() => 
                val (e, s2) = parse_decision(s1)
                val s3 = parse((T_RPAR(), s2))
                (e, s3)
              case _ => 
                parse_condition(s)
              }
        }
      }
      result
    }
  }
  def parse_condition: (List[(Token, Int)]) => (Exp, List[(Token, Int)]) = {
    (s: List[(Token, Int)]) => {
      val (e1, s1) = parse_exp(s)
      s1 match {
        case Nil => 
          throw new Exception("Unexpected EOF")
        case (h :: s2) => 
          def result: (Operation, List[(Token, Int)]) = {
            val (tok, line) = h
            tok match {
              case T_EQUAL() => 
                (Equal(), s2)
              case T_LESS() => 
                (Less(), s2)
              case T_GREATER() => 
                (Greater(), s2)
              case T_LESSEQ() => 
                (LessEq(), s2)
              case T_GREATEREQ() => 
                (GreaterEq(), s2)
              case _ => 
                throw new Exception(("Syntax error line " + String.valueOf(line)))
              }
          }
          val (operation, s3) = result
          val (e2, s4) = parse_exp(s3)
          def e: Exp = {
            Oper(operation, e1, e2)
          }
          (e, s4)
      }
    }
  }
  def parse_exp: (List[(Token, Int)]) => (Exp, List[(Token, Int)]) = {
    (s: List[(Token, Int)]) => {
      def result: (Exp, List[(Token, Int)]) = {
        val (e1, s1) = parse_term(s)
        s1 match {
          case Nil => 
            throw new Exception("Unexpected EOF")
          case (h :: s2) => 
            val (tok, _) = h
            tok match {
              case T_PLUS() => 
                val (e2, s3) = parse_term(s2)
                (Oper(Plus(), e1, e2), s3)
              case T_MINUS() => 
                val (e2, s3) = parse_term(s2)
                (Oper(Minus(), e1, e2), s3)
              case _ => 
                (e1, s1)
              }
        }
      }
      result
    }
  }
  def parse_term: (List[(Token, Int)]) => (Exp, List[(Token, Int)]) = {
    (s: List[(Token, Int)]) => {
      def result: (Exp, List[(Token, Int)]) = {
        val (e1, s1) = parse_factor(s)
        s1 match {
          case Nil => 
            throw new Exception("Unexpected EOF")
          case (h :: s2) => 
            val (tok, _) = h
            tok match {
              case T_TIMES() => 
                val (e2, s3) = parse_factor(s2)
                (Oper(Times(), e1, e2), s3)
              case T_SLASH() => 
                val (e2, s3) = parse_factor(s2)
                (Oper(Slash(), e1, e2), s3)
              case _ => 
                (e1, s1)
              }
        }
      }
      result
    }
  }
  def parse_factor: (List[(Token, Int)]) => (Exp, List[(Token, Int)]) = {
    (s: List[(Token, Int)]) => {
      def result: (Exp, List[(Token, Int)]) = {
        s match {
          case Nil => 
            throw new Exception("Unexpected EOF")
          case (h :: s1) => 
            val (tok, line) = h
            tok match {
              case T_Ident(v) => 
                (Var(v), s1)
              case T_Int(i) => 
                (IntCst(i), s1)
              case T_LPAR() => 
                val (e1, s2) = parse_exp(s1)
                val s3 = parse((T_RPAR(), s2))
                (e1, s3)
              case _ => 
                throw new Exception(("Syntax error line " + String.valueOf(line)))
              }
        }
      }
      result
    }
  }
  def parseComm: (List[(Token, Int)]) => (Comm, List[(Token, Int)]) = {
    (s: List[(Token, Int)]) => {
      def result: (Comm, List[(Token, Int)]) = {
        val (c, s2) = parseComm2(s)
        val line = getLine(s)
        val line2 = s2 match {
          case Nil => 
            getLastLine(s)
          case (_ :: _) => 
            getLine(s2)
        }
        (CommRegion(c, line, line2), s2)
      }
      result
    }
  }
  def parseComm2: (List[(Token, Int)]) => (Comm, List[(Token, Int)]) = {
    (s: List[(Token, Int)]) => {
      def result: (Comm, List[(Token, Int)]) = {
        s match {
          case Nil => 
            throw new Exception
          case (h :: s0) => 
            val (tok, line) = h
            tok match {
              case T_BEGIN() => 
                val (l, s1) = parseCommList(s0)
                (make_seq(l), s1)
              case T_WHILE() => 
                val (e, s2) = parse_decision(s0)
                val s3 = parse((T_LOOP(), s2))
                val (l, s4) = parseCommList(s3)
                val s5 = parse((T_LOOP(), s4))
                (While(e, make_seq(l)), s5)
              case T_IF() => 
                val (e, s2) = parse_decision(s0)
                val s3 = parse((T_THEN(), s2))
                val (l1, s4) = parseCommList(s3)
                s4 match {
                  case Nil => 
                    throw new Exception("Unexpected EOF")
                  case (h2 :: s5) => 
                    val (tok2, _) = h2
                    tok2 match {
                      case T_ELSE() => 
                        val (l2, s6) = parseCommList(s5)
                        val s7 = parse((T_IF(), s6))
                        (IfThenElse(e, make_seq(l1), make_seq(l2)), s7)
                      case _ => 
                        val s6 = parse((T_IF(), s4))
                        (IfThen(e, make_seq(l1)), s6)
                      }
                }
              case T_RETURN() => 
                val (_, s1) = parse_exp(s0)
                (Null(), s1)
              case T_Ident(v) => 
                val s1 = parse((T_ASSIGN(), s0))
                val (e, s2) = parse_exp(s1)
                (Assign(v, e), s2)
              case _ => 
                throw new Exception(("Syntax error line " + String.valueOf(line)))
              }
        }
      }
      result
    }
  }
  def parseCommList: (List[(Token, Int)]) => (List[Comm], List[(Token, Int)]) = {
    (s: List[(Token, Int)]) => {
      def result: (List[Comm], List[(Token, Int)]) = {
        s match {
          case Nil => 
            throw new Exception("Unexpected EOF")
          case (h :: s0) => 
            val (tok, _) = h
            tok match {
              case T_END() => 
                (Nil, s0)
              case T_ELSE() => 
                (Nil, s)
              case _ => 
                val (i, s1) = parseComm(s)
                val s2 = parse((T_SEMI(), s1))
                val (l, s3) = parseCommList(s2)
                ((i::l), s3)
              }
        }
      }
      result
    }
  }
  def parseProg: (List[(Token, Int)]) => Comm = {
    (s: List[(Token, Int)]) => {
      s match {
        case Nil => 
          throw new Exception("Unexpected EOF")
        case (h :: s0) => 
          val (tok, _) = h
          tok match {
            case T_BEGIN() => 
              val (l, s1) = parseCommList(s0)
              val s2 = parse((T_SEMI(), s1))
              s2 match {
                case Nil => 
                  make_seq(l)
                case (_ :: _) => 
                  throw new Exception("EOF expected")
              }
            case _ => 
              parseProg(s0)
            }
      }
    }
  }
  
  def parseFile(filename: String) : Comm  = {
    val strm = new FileReader(filename)
    val content = Lexer.read_all(strm)
    val tokens = Lexer.lexer(content)
    parseProg(tokens)
  }
  
  def main(args: Array[String]) : Unit  = {
    print(parseFile("resources/test1.adb"))
  }
  
}

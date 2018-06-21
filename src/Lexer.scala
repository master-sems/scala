import java.io.Reader
import java.io.FileReader
import java.io.IOException
import java.util.NoSuchElementException

import Util._

abstract class Token
case class T_Int (field1: Int) extends Token
case class T_Ident (field1: String) extends Token
case class T_PROCEDURE () extends Token
case class T_IS () extends Token
case class T_IN () extends Token
case class T_OUT () extends Token
case class T_WHILE () extends Token
case class T_IF () extends Token
case class T_THEN () extends Token
case class T_ELSE () extends Token
case class T_RETURN () extends Token
case class T_BEGIN () extends Token
case class T_END () extends Token
case class T_LOOP () extends Token
case class T_AND () extends Token
case class T_OR () extends Token
case class T_PLUS () extends Token
case class T_MINUS () extends Token
case class T_TIMES () extends Token
case class T_SLASH () extends Token
case class T_EQUAL () extends Token
case class T_LESS () extends Token
case class T_GREATER () extends Token
case class T_LESSEQ () extends Token
case class T_GREATEREQ () extends Token
case class T_LPAR () extends Token
case class T_RPAR () extends Token
case class T_LCURL () extends Token
case class T_RCURL () extends Token
case class T_LSQUARE () extends Token
case class T_RSQUARE () extends Token
case class T_COMMA () extends Token
case class T_COLON () extends Token
case class T_SEMI () extends Token
case class T_ASSIGN () extends Token

object Lexer {

  def symbols: List[(String, Token)] = {
    List((":=", T_ASSIGN()), ("<=", T_LESSEQ()), (">=", T_GREATEREQ()), (":", T_COLON()), (",", T_COMMA()), ("=", T_EQUAL()), ("+", T_PLUS()), ("-", T_MINUS()), ("*", T_TIMES()), ("/", T_SLASH()), (";", T_SEMI()), ("<", T_LESS()), (">", T_GREATER()), ("(", T_LPAR()), (")", T_RPAR()), ("{", T_LCURL()), ("}", T_RCURL()), ("[", T_LSQUARE()), ("]", T_RSQUARE()))
  }
  def keywords: List[(String, Token)] = {
    List(("procedure", T_PROCEDURE()), ("is", T_IS()), ("in", T_IN()), ("out", T_OUT()), ("while", T_WHILE()), ("if", T_IF()), ("then", T_THEN()), ("else", T_ELSE()), ("end", T_END()), ("begin", T_BEGIN()), ("loop", T_LOOP()), ("and", T_AND()), ("or", T_OR()), ("return", T_RETURN()))
  }
  val tokens_to_symbols_and_keywords = switch((symbols ++ keywords))
  
  def token_to_string(t: Token) : String  = {
    t match {
      case T_Int(i) => 
        String.valueOf(i)
      case T_Ident(v) => 
        v
      case _ => 
        assoc(tokens_to_symbols_and_keywords, t)
      }
  }
  
  def implode(l: List[Char]) : String  = {
    l match {
      case Nil => 
        ""
      case (h :: t) => 
        (String.valueOf(h) + implode(t))
    }
  }
  
  def explode_rec(s: String, i: Int) : List[Char]  = {
    if (i < s.length) {
      (s.charAt(i)::explode_rec(s, (i + 1)))
    } else {
      Nil
    }
  }
  
  def explode(s: String) : List[Char]  = {
    explode_rec(s, 0)
  }
  
  def is_prefix(l1: List[Char], l2: List[Char]) : Boolean  = {
    l1 match {
      case Nil => 
        true
      case (h1 :: t1) => 
        l2 match {
          case Nil => 
            false
          case (h2 :: t2) => 
            if (h1 == h2) {
              is_prefix(t1, t2)
            } else {
              false
            }
        }
    }
  }
  
  def find_symbol(sl: List[(String, Token)], l: List[Char]) : List[Char]  = {
    sl match {
      case Nil => 
        throw new NoSuchElementException
      case (h :: tl) => 
        val (s, _) = h
        val cl = explode(s)
        if (is_prefix(cl, l)) {
          cl
        } else {
          find_symbol(tl, l)
        }
    }
  }
  
  def cut_symbol(l: List[Char]) : (List[Char], List[Char])  = {
    val cl = find_symbol(symbols, l)
    val n = cl.length
    (cl, discard(n, l))
  }
  
  def make_symbol(s: String) : Token  = {
    Util.assoc(symbols, s)
  }
  
  def splitl_rec[A] (p: (A) => Boolean, l: List[A], l2: List[A]) : (List[A], List[A])  = {
    l2 match {
      case Nil => 
        (l, Nil)
      case (x :: tl) => 
        if (p(x)) {
          splitl_rec(p, (l ++ List(x)), tl)
        } else {
          (l, l2)
        }
    }
  }
  
  def splitl[A] (p: (A) => Boolean, l: List[A]) : (List[A], List[A])  = {
    splitl_rec(p, Nil, l)
  }
  
  def chr(x: Int) : Char  = {
    x.toChar
  }
  
  def code(x: Char) : Int  = {
    x.toInt
  }
  
  def read_all(strm: Reader) : List[Char]  = {
    try {
      val b = strm.read
      if (b < 0) {
        Nil
      } else {
        (chr(b)::read_all(strm))
      }
    } catch {
      case exn : IOException => Nil
    }
  }
  val is_letter = (c: Char) => {
    ((!(c < 'a')) && (!(c > 'z')))
  }
  val is_cap = (c: Char) => {
    ((!(c < 'A')) && (!(c > 'Z')))
  }
  val is_digit = (c: Char) => {
    ((!(c < '0')) && (!(c > '9')))
  }
  val is_punct = (c: Char) => {
    (((!(c < '!')) && (!(c > '~'))) || (c == '&'))
  }
  val is_alpha = (c: Char) => {
    (is_letter(c) || is_cap(c))
  }
  val is_alpha_num = (c: Char) => {
    ((c == '_') || (is_alpha(c) || is_digit(c)))
  }
  val is_newline = (c: Char) => {
    (code(c) == 10)
  }
  
  def scan_num(l: List[Char]) : Token  = {
    T_Int(implode(l).toInt)
  }
  
  def scan_symb(l: List[Char]) : Token  = {
    make_symbol(implode(l))
  }
  
  def scan_alpha_num(l: List[Char]) : Token  = {
    val s = implode(l)
    if (Util.mem_assoc(keywords, s)) {
      Util.assoc(keywords, s)
    } else {
      T_Ident(s)
    }
  }
  
  def scan(line: Int, s: List[Char]) : List[(Token, Int)]  = {
    s match {
      case Nil => 
        Nil
      case (c :: s1) => 
        if (is_alpha(c)) {
          val p = splitl(is_alpha_num, s)
          val (l, s2) = p
          ((scan_alpha_num(l), line)::scan(line, s2))
        } else {
          if (is_digit(c)) {
            val p = splitl(is_digit, s)
            val (l, s2) = p
            ((scan_num(l), line)::scan(line, s2))
          } else {
            if (is_punct(c)) {
              val p = cut_symbol(s)
              val (l, s2) = p
              ((scan_symb(l), line)::scan(line, s2))
            } else {
              if (is_newline(c)) {
                scan((line + 1), s1)
              } else {
                scan(line, s1)
              }
            }
          }
        }
    }
  }
  
  def lexer(s: List[Char]) : List[(Token, Int)]  = {
    scan(1, s)
  }
  
  def parseFile(filename: String) : List[(Token, Int)]  = {
    val strm = new FileReader(filename)
    val content = read_all(strm)
    val tokens = lexer(content)
    tokens
  }
  
  def main(args: Array[String]) : Unit  = {
    print(parseFile("resources/test1.adb"))
  }
  
}

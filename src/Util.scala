import java.io.Reader
import java.io.FileReader
import java.io.IOException
import java.util.NoSuchElementException


object Util {

  
  def fst[A, B] (z: (A, B)) : A  = {
    val (x, _) = z
    x
  }
  
  def snd[A, B] (z: (A, B)) : B  = {
    val (_, y) = z
    y
  }
  
  def member[A] (x: A, l: List[A]) : Boolean  = {
    l match {
      case Nil => 
        false
      case (h :: t) => 
        if (x == h) {
          true
        } else {
          member(x, t)
        }
    }
  }
  
  def is_empty[A] (l: List[A]) : Boolean  = {
    l match {
      case Nil => 
        true
      case (_ :: _) => 
        false
    }
  }
  
  def discard[A] (n: Int, l: List[A]) : List[A]  = {
    l match {
      case Nil => 
        Nil
      case (_ :: t) => 
        if (n == 0) {
          l
        } else {
          discard((n - 1), t)
        }
    }
  }
  
  def nth[A] (l: List[A], n: Int) : A  = {
    l match {
      case Nil => 
        throw new NoSuchElementException
      case (h :: t) => 
        if (n == 0) {
          h
        } else {
          nth(t, (n - 1))
        }
    }
  }
  
  def combine[A, B] (l1: List[A], l2: List[B]) : List[(A, B)]  = {
    l1 match {
      case Nil => 
        l2 match {
          case Nil => 
            Nil
          case (_ :: _) => 
            Nil
        }
      case (h1 :: t1) => 
        l2 match {
          case Nil => 
            Nil
          case (h2 :: t2) => 
            ((h1, h2)::combine(t1, t2))
        }
    }
  }
  
  def split[A, B] (l: List[(A, B)]) : (List[A], List[B])  = {
    l match {
      case Nil => 
        (Nil, Nil)
      case (h :: t) => 
        val (l1, l2) = split(t)
        val (h1, h2) = h
        ((h1::l1), (h2::l2))
    }
  }
  
  def switch[A, B] (l: List[(A, B)]) : List[(B, A)]  = {
    val (l1, l2) = split(l)
    combine(l2, l1)
  }
  
  def assoc[B, A] (l: List[(A, B)], x: A) : B  = {
    l match {
      case Nil => 
        throw new NoSuchElementException
      case (h :: t) => 
        val (k, v) = h
        if (x == k) {
          v
        } else {
          assoc(t, x)
        }
    }
  }
  
  def mem_assoc[B, A] (l: List[(A, B)], x: A) : Boolean  = {
    l match {
      case Nil => 
        false
      case (h :: t) => 
        val (k, _) = h
        if (x == k) {
          true
        } else {
          mem_assoc(t, x)
        }
    }
  }
  
  def map[B, A] (f: (A) => B, l: List[A]) : List[B]  = {
    l match {
      case Nil => 
        Nil
      case (h :: t) => 
        (f(h)::map(f, t))
    }
  }
  
  def reverse[A] (l: List[A]) : List[A]  = {
    l match {
      case Nil => 
        Nil
      case (h :: t) => 
        (reverse(t) ++ List(h))
    }
  }
  
}

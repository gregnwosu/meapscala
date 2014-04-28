
object Hello{
 
 def main(args: Array[String]) :Unit  = {
   println("Hello World")
   println("this is a test")
   println(List(1,2,3,4) )

   val x = List(1,2,3,4,5) match {
     case Cons(x, Cons(2, Cons(4, _))) => x
     case Nil => 42
     case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
     case _ => 101
   }

   println (   
foldLeft(List(1,2,3),
Nil:List[Int])
((a:List[Int], b:Int)=> Cons(b,a)))
}





def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
as match {
case Nil => z
case Cons(x, xs) => f(x, foldRight(xs, z)(f))
}

def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = 
l match {
  case Nil => z
  case Cons(x,xs) => foldLeft( xs, f(z, x)) (f)
  
}

foldRight(List(1,2,3),
Nil:List[Int])(Cons(_,_))

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

def sum(ints: List[Int]): Int = ints match {
  case Nil => 0
  case Cons(x,xs) => x + sum(xs)
}
 
def tail[A](ds:List[A]):List[A] = ds match {
  case Nil => Nil
  case Cons(x,xs) => xs
}

def drop[A](ds:List[A], n:Int):List[A] = ds match {
  case Nil => Nil
  case Cons(x,xs) => if (n==0) xs else drop(xs, n-1)
}

def init[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case Cons (x,Nil) => Nil
  case Cons(x,xs) => Cons(x,init(xs))
}

def dropWhile[A](ds:List[A], f: A=>Boolean):List[A] = ds match {
  case Nil => Nil
  case Cons(x,xs) => if (f(x)) Cons(x,xs) else dropWhile(xs,f)
}
def setHead[A](ds:List[A],h:A):List[A] = Cons(h,ds)

def product(ds: List[Double]): Double = ds match {
  case Nil => 1.0
  case Cons(0.0, _) => 0.0
  case Cons(x,xs) => x * product(xs)
}

def apply[A](as: A*): List[A] =
  if (as.isEmpty) Nil
  else Cons(as.head, apply(as.tail: _*))
}

 def partial[A,B,C](a:A , f:  (A,B) => C ):  (B => C) =
 (b:B) => f (a, b)



def curry[A,B,C](f:(A,B)=>C):A => (B => C) =
 (a:A) =>  (b:B) => f(a,b)




def uncurry[A,B,C](f: A => B=> C): (A,B)=>C =
(a:A,b:B) => f (a) (b)

def compose[A,B,C](f: A=> B, g: B=>C):A=>C =
  (a:A) => g (f(a))
}

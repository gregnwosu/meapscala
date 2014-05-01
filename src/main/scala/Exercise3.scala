
object Hello{
 
 def oldmain(args: Array[String]) :Unit  = {
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

println(append(List(1,2,3),List(4,5,6)))
println(concat(List(List(1,2,3), List(4,5,6))))

println(addOne(List(1,2,3)))
println(filterB(List(1,20,33)) (_ > 1))
println(foldRight(List(3,2,3), Nil:List[String])((a:Int,b:List[String])=>Cons(a.toString,b)))
println(List("12","34"))
println(flatMap(List(1,2,3))(i => List(i,i)))
println(addLists(List(10,20,30), List(4,5,6)) ((_+_)))



}


def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B]) ((a:A, b:List[B]) => Cons(f(a),b))
def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, Nil:List[A]) ((a:A, b:List[A]) => 
  if (f(a)) Cons(a,b) else b)
def filterB[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)((a:A)=>if (f(a)) List(a) else Nil)
def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = concat( map(l)(f))
def addOne(l:List[Int]):List[Int] =
foldRight(l,Nil:List[Int])((a:Int,b:List[Int])=> Cons(a+1,b))

def head[A] (l:List[A]) : A = l match {
  case Cons(x,xs) => x
}


//def hasSubsequence[A](l: List[A], sub: List[A]): Boolean  =  scanRight (l,Nil:List[A])  ((a:A, b:List[B]) => Cons(a,b)) exists ((c:List[A]) => c == l)

def tail[A] (l:List[A]) : List[A] = l match {
  case Nil => Nil
  case Cons(x,xs) => xs
}


def addLists[A](a:List[A], b:List[A])(f: (A,A) => A): List[A] = a match { 
  case Nil => b
  case Cons(x,xs) => 
    Cons(f(x, head(b)), addLists(xs, tail(b))(f))
                     
}


def concat[A](l:List[List[A]]):List[A] =
foldLeft (l,Nil:List[A]) (append(_, _))

def append[A](l1:List[A],l2:List[A]):List[A] =
foldRight(l1, l2)((Cons(_,_)))


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

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def size[A](t:Tree[A]):Int = t match {
  case Leaf(_) => 1
  case Branch(l,r) => 1 + size(l) + size(r)
}

def max(a:Int, b:Int) : Int = if (a > b) a else b

/*def maximum[Int](t:Tree[Int]): Int = t match {
  case Leaf(v)     =>  v
  case Branch(l,r) =>  maximum(l) max maximum(r)
}

def maxdepth[Int](t:Tree[Int]):Int = t match {
  case Leaf(v)     =>  1
  case Branch(l,r) =>  (1 + maxdepth(l)) max (1 + maxdepth(r))
}*/

def mapTree[A,B](t:Tree[A], f: A => B):Tree[B] = t match {
  case Leaf(v)     => Leaf (f(v))
  case Branch(l,r) => Branch ((mapTree(l,f)) , (mapTree(r,f)))
}


def foldTree[A,B](a:Tree[A], b:B) ( f : (A,B) => B):B = a match {
case Leaf(v) => f (v,b)
case Branch(l, r) => foldTree(r, (foldTree(l, b) (f))) (f)
}


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

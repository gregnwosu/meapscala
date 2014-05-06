object Exercise5{



sealed trait Stream[+A]  {

    def empty[A]: Stream[A] = Empty
         
   
    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def headOptionold:Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }
    def toList:List[A]  = this match {
      case Empty => List()
      case Cons(h,t) =>  h() :: t().toList
    }

    def takeOld(n:Int):Stream[A] = this match {
      case Empty => Empty
      case Cons(h,t) => if (n<0) Empty else cons(h(),   t().take(n-1))
    }

    def drop(n:Int):Stream[A] = this match {
      case Empty => Empty
      case Cons(h,t) => if (n<0) Empty else t().drop(n-1)
    }

   def takeWhileold(p: A => Boolean): Stream[A] = this match {
     case Empty => Empty
     case Cons(h,t) => if (! p(h())) Empty else cons(h(),t().takeWhileold(p))

  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean = foldRight (true) ((a,b) => p(a) && b)
  def headOption:Option[A] = foldRight (None:Option[A]) ((a,b) => if (a==Empty) None else (Some(a)))
  def takeWhileOld(p: A => Boolean): Stream[A] = foldRight (Empty:Stream[A]) ((a,b) => if (!p(a)) Empty else  cons(a,b) )
  def mapOld[B](f: A => B):Stream[B] = foldRight (Empty:Stream[B]) ( (a,b) => cons ( f(a), b))
  def filter(f: A => Boolean):Stream[A] = foldRight (Empty:Stream[A]) ( (a,b) => if (!f(a) ) b else cons (a,b))
  def append[B](b:Stream[B]):Stream[A] = foldRight (Empty:Stream[A]) ( (a,b) => if (a==Empty) b else cons (a,b))
 
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight (Empty:Stream[B]) ((a,b) => f(a) append( b))

  def take(n:Int):Stream[A] = unfold ((n, this)) ( t => t match{
    case (_, Empty)     => None
    case (n, Cons(h,t)) => if (n<=0) None else Some((h(),(n-1,t())))
}) 
 
 


  def takeWhile(p: A => Boolean): Stream[A] = unfold (this) (s => s match {
    case Empty => None
    case Cons(h,t) => if (!p(h())) None else Some((h(), t()))
} )

  def map[B](f: A => B):Stream[B] = unfold (this) ( s => s  match{
    case Empty => None
    case Cons(h,t) => Some(f(h()), t())
  })



def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])]= unfold ((this,s2)) (t =>  t match {
  case (Empty, Empty)              => None
  case (Empty, Cons(h,t))          =>  Some(((None, Some(h())  ), (Empty  , t()    )   ) )
  case ( Cons(h,t), Empty)         =>  Some(((Some(h()) , None ), (t()    , Empty  )   ) )
  case ( Cons(h1,t1), Cons(h2,t2)) =>  Some(((Some(h1()) , Some(h2()) ), (t1()  , t2()   )   ) )
})

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
def constantold[A](a: A): Stream[A] = cons(a, constantold(a))
def fromold(n: Int): Stream[Int] = cons(n, fromold(n+1))   
def fibsold(): Stream[Int] = {
  def fib(m:Int, n :Int):Stream[Int] = cons(m,  fib(n,m+n)) 
fib(0,1)
}


def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
  case None  => Empty
  case Some((ans, seed)) => cons(ans,unfold(seed)(f))
}

//fibs , from , constant , and ones
def constant[A](a: A): Stream[A] = unfold (a) (x => Some((a,a)))
def ones(): Stream[Int] = unfold (1) (_ => Some((1,1)))
def from(n: Int): Stream[Int] = unfold (n) ( x => Some((x, x+1)) )
def fibs(): Stream[Int] = {
  def fib(m:Int, n :Int):Stream[Int] = unfold ((m,n)) ((t) => t match {
    case (tm,tn) => Some(((tm), (tn, tn + tm))) 
})
fib(0,1)
}

object Stream{
    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def empty[A]: Stream[A] = Empty

}


def main(args: Array[String]) :Unit  = {
    println(Stream(1,2,3).drop(1).toList)
    println(Stream(1,2,3).takeWhile(_ < 3).toList)
    println(Stream(1,2,3).filter(_ == 2).toList)
    println(Stream(1,2,3).headOption)
    println(constant(3).take(6).toList)
    println(fibs().take(6).toList)
    println(Stream(1,2,3).append(Stream(4,5,6)).toList)
    println( Stream(1,2,3).zipAll(Stream(4,5,6,7)).toList  )
  }

}

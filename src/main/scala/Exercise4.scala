import scala.math.pow

object Exercise4{

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
      case Right(v) => Right (f(v))
      case Left(v) => Left(v)
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = (this map(f)) match {
    case Right(Right(v)) => Right(v)
    case Right(Left(v)) => Left(v)
    case Left(v) => Left(v)

  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(v) => Right(v)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):Either[EE, C] = 
   this flatMap ( (r1v => (b map ( r2v  => f (r1v,r2v)))))

def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
  traverse (es) (x => x)


def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
as match {
  case Nil  => Right(Nil)
  case x::xs => f(x) flatMap (  (fx =>    traverse(xs)(f)  map ((fx::_))   ))
  }

  
}


  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
      case Some(v) => Some (f(v))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = (this map(f)) match {
      case Some(Some(v)) => Some(v)
      case Some(None) => None
      case _ => None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(v) => v
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = if (this==None) ob else this


    def filter(f: A => Boolean): Option[A] =
      if (map(f) getOrElse(false))
        this
      else
        None
  }


  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty)
      None
    else
      Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    
    val om = mean(xs)
    val ol = om map (varianceelem(xs))
    ol flatMap (mean)
    
  }

 def varianceelem(xs:Seq[Double]) (m:Double):Seq[Double] =
   xs map ((el:Double) => pow(el-m,2))
 

def sequenceold[A](a: List[Option[A]]): Option[List[A]] = a match  {
  case Nil  => Some(Nil)
  case None::xs => None
  case x::xs => map2(x,sequenceold(xs))(((_::_)))
}


def sequence   [A](a: List[Option[A]]): Option[List[A]] = 
traverse (a)(x => x)

def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
  case Nil  => Some(Nil)
  case x::xs => f(x) flatMap (  (fx:B) =>    traverse(xs)(f)  map ((fx::_))   )
  }



def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
if ((a==None) || (b==None) )
  None
else 
  Some(f(a getOrElse(throw new Exception()), b getOrElse(throw new Exception())) )

def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def main(args: Array[String]) :Unit  = {
    println(variance(Seq(2.0,4.0,6.0,8.0) ))
    println(sequenceold(List(Some(2),Some(4)) ))
    println(sequence   (List(Some(2),Some(4)) ))
  }


}


/* would create a new datatype, similar to the monadic writer type so errors could be logged, i guess i would make the new log also some kind of monoid so that i would know how to append it */

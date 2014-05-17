
// my answer to exercise 1
//map2[B,C](pb:Par[B], f: (a:=>A,b:=>B) => C):Par[C]

import java.util.concurrent.Callable
import java.util.concurrent.TimeUnit
import java.util.concurrent.Future

// trait Future[A] {
//     def get: A
//     def get(timeout: Long, unit: TimeUnit): A
//     def cancel(evenIfRunning: Boolean): Boolean
//     def isDone: Boolean
//     def isCancelled: Boolean
// }



class Exercise7{
  type Par[A] = ExecutorService => Future[A]
  abstract class ExecutorService {
    def submit[A](a: Callable[A]): Future[A]
  }

  

object Par {
  case class UnitFuture[A](f:(Long,TimeUnit) => A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = f(timeout, units)
    def get = f(9999999, TimeUnit.HOURS)
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture((_,_)=> a)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      val tf = (timeout:Long, units:TimeUnit) => {
      val ms = TimeUnit.MILLISECONDS.convert(timeout, units)
      val now = System.currentTimeMillis
      val av = af.get(ms,TimeUnit.MILLISECONDS)
      val after = System.currentTimeMillis
      val elapsedms = after - now
      val remainingms = ms - elapsedms
      val bv = bf.get(remainingms, TimeUnit.MILLISECONDS)
      f(av,bv)
      }
      
      UnitFuture(tf)
    }

  def asyncF[A,B](f: A => B): A => Par[B] = (a:A) => lazyUnit(f(a))

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def parMap[A,B](l: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = l.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    ((es:ExecutorService) => {
      unit(
          l map((pa:Par[A])=>pa(es).get)
        )(es)
    })
     
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] =   {
    val fbs: List[Par[A]] = l.map(lazyUnit(_))

    ((es:ExecutorService) =>  
      unit(
        
          fbs.foldRight (Nil:List[A]) 
            (
              (pa:Par[A], la:List[A])=>  f (pa(es).get) match{
                case true => pa(es).get::la
                case false => la
               
              }
            )
        
      )(es)
    )
}

def chooser[A,B] (n:Par[A])(choices:A =>Par[B]):Par[B] = es => choices(n(es).get)(es)

def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A]   =  chooser(n)(choices(_))

def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V]=  es => choices(key(es).get)(es)

def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = chooser(cond)(b => if(b)t else f)

def join[A](ppa: Par[Par[A]]): Par[A] = es => {
  val pa = ppa(es).get
  pa(es)
}

def flatMap[A,B] (n:Par[A])(f:A =>Par[B]):Par[B] = join(es  =>  unit(f(n(es).get))(es))

}
}

//Exercise7 

/*
Given 
1. map(y)(id) == y ,
show that 
2. map(map(y)(g))(f) == map(y)(f compose g) .

for f = id

map (map(y)(g)) (id)  
 = map(y)(g) [substituting into 1]
 = map(y)(id compose g)
 = map(y)(f compose g) 2.

map ([x]) (f) = [f(x)]
therefore 
map (map(y)(g)) (f) 
 = [f(map(y)(g))] 
 = [f([g(y)])]
 = [ f . g (y) ]
 = map (y) (f .g ) from 2.
 */
   
/* Exercise 9
fork(n) uses 1 additional thread from the main thread when called
fork(n) deadlocks when numThreads = 1
for a n size threadpool, fork n creates n threads
*/    

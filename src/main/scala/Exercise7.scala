
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



class Exercise{
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

  def sequence[A](l: List[Par[A]]): Par[List[A]] =(es:ExecutorService) =>  UnitFuture((_,_)=>l map((pa:Par[A])=>pa(es).get))
     
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] =   {
    val fbs: List[Par[B]] = l.map(asyncF(f))

  }


}

}

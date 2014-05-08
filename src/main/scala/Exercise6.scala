



case class Simple(seed: Long) extends RNG {
      def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = Simple(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
      }
    }


trait RNG {
  def nextInt: (Int, RNG)
  }
  


case class State[S,+A](run: S => (A,S)) {
 
  
}

object Exercise6 {
  type State[S,+A] = S => (A,S)
  type Rand[A] = State[RNG,A]

  //def get[S]: State[S, S] = State(s => (s, s))
  //def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def mapold[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2old[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
   rng => {
     val (a, rng2) = ra(rng)
     val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
   }


  def flatMap[S,A,B](f: State[S,A])(g: A => State[S,B]): State[S,B] = 
    rng => {
      val (ra, rng1) = f(rng)
      val (rb, rng2) = g(ra)(rng1)
      (rb,rng2)
    }
  def unit[S,A](a: A): State[S, A] =
    rng => (a, rng)

  def map[S,A,B](s: State[S,A])(f: A => B): State[S,B] = flatMap (s) ( x =>unit(f(x)))

  def map2[S,A,B,C](ra: State[S,A], rb: State[S,B])(f: (A, B) => C): State[S,C] = 
    flatMap (ra) ((a:A) => map (rb) ((b:B) => f(a,b) ))


  def sequence[S,A](fs: List[State[S,A]]): State[S, List[A]] = 
    rng => {

      def processRands(gs: List[State[S,A]])(rg:S):(List[A],S) = gs match {
        
        case h::t => {
          val (lh,rng2) = h(rg)
          val (lt,rng3) = processRands(t)(rng2)
          (lh::lt,rng3)
        }
        case _ => (List() ,rng)
      }
      processRands(fs)(rng)
      
    }

def nonNegativeLessThan(n: Int): Rand[Int] = 
  flatMap(nonNegativeInt) (i => rng =>{ 
    val mod = i % n
    if (i + (n-1) - mod >= 0) 
      (mod, rng)
    else 
      nonNegativeLessThan(n)(rng)
})

def rollDie: Rand[Int] = nonNegativeLessThan(6)

def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
 val nextIntFunc : Rand[Int] = (_.nextInt)
 val funcList : List[Rand[Int]] = List.fill(count)(nextIntFunc)
 sequence(funcList)(rng)
}

 
  val range = Int.MaxValue.toDouble  - Int.MinValue.toDouble
  val delta = 1 / range

  def nonNegativeInt(rng: RNG):(Int,RNG)={
    val  (res, rng2) = rng.nextInt
    if (res == Int.MinValue)
      nonNegativeInt(rng2)
    else
      (Math.abs(res), rng2)
  }



  def double(rng: RNG): (Double, RNG)=
    map (nonNegativeInt) (_ * delta) (rng)



  def intsold(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (List(),rng)
    case c => {
      val (iv,rng2) = rng.nextInt
      val (il,rng3) = ints(c -1)(rng2)
      (iv::il,rng3)
    }
  }




def intDouble(rng: RNG): ((Int,Double), RNG) = {
   val (iv,rng2) = nonNegativeInt(rng)
   val (dv,rng3) = double(rng2)
   ((iv,dv), rng3)
}

def doubleInt(rng: RNG): ((Double,Int), RNG) = {
   val (iv,rng2) = nonNegativeInt(rng)
   val (dv,rng3) = double(rng2)
   ((dv,iv), rng3)
}
def double3(rng: RNG): ((Double,Double,Double), RNG) = {
  val (dv1,rng2) = double(rng)
  val (dv2,rng3) = double(rng2)
  val (dv3,rng4) = double(rng3)
  ((dv1,dv2,dv3), rng4)
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int) {

def insert(n:Int):State[(Boolean,Int,Int),Int] =
(state:(Boolean,Int,Int)) => state match {
   case (_, 0, _) => (0,state)
   case (false,_,_) => (0,state)
   case (true,candies,coins) => (0,(false,candies,coins+n))
 }


def turn:State[(Boolean,Int,Int),Int] =
(state:(Boolean,Int,Int)) => state match {
   case (_, 0, _) => (0,state)
   case (false,candies,coins) => (1,(true,candies-1, coins))
   case (true,_,_) => (0,state)
 }



}




def main(args: Array[String]) :Unit  = {
    val seed = Simple(42)
    println("%.3f".format(double(seed)._1))
    println(ints(4)(seed))
    println(nonNegativeInt(seed)._1)
    // for{
    //  _ <- set((true,50,0))
    //  _ <-insert(2)
    //  a <- turn
    // }yield(a)
}
  

}





abstract class Exercise8 {
def listOf[A](a: Gen[A]): Gen[List[A]]
def forAll[A](l:Gen[List[A]]) (f : A => Boolean):Prop



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

type Rand[A] = State[RNG,A]
type State[S,+A] = S => (A,S)
case class Gen[A](sample: State[RNG,A]){
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen((s:RNG) =>  {
   val (a,s1) = sample (s)
   val f2 = f(a).sample
   f2(s1)
  })

def map[B](f: A => B): Gen[B]  = this flatMap ((a:A) => unit(f(a))) 

def map2[B,C](rb:Gen[B])(f: (A,B)=>C): Gen[C] = flatMap ((a:A) => rb map  ((b:B) => f(a,b) ))

def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap ((sz:Int)=>listOfNStatic(sz)(this))


}

  

def unit[A](a: => A):Gen[A] = Gen((s:RNG) => (a,s))
def boolean: Gen[Boolean] = Gen(map (((x:RNG) => x.nextInt):State[RNG,Int])  (_%2==0))

def listOfNStatic[A](n: Int)(g: Gen[A]): Gen[List[A]]  = Gen(sequence (List.fill(n)(g.sample)))


trait Prop { 
  def && (p: Prop): Prop  = forAll(unit( List(this, p)))(_.check)
  def check : Boolean
}
    
 
//  def check: Either[(FailedCase, SuccessCount), SuccessCount]



object Gen{
  def nonNegativeInt(rng: RNG):(Int,RNG)={
    val  (res, rng2) = rng.nextInt
    if (res == Int.MinValue)
      nonNegativeInt(rng2)
    else
      (Math.abs(res), rng2)
  }

def nonNegativeLessThan(n: Int): Rand[Int] = 
  flatMap(nonNegativeInt) (i => rng =>{ 
    val mod = i % n
    if (i + (n-1) - mod >= 0) 
      (mod, rng)
    else 
      nonNegativeLessThan(n)(rng)
})

def choose(start: Int, stopExclusive: Int): Gen[Int]  = {
  val dist = stopExclusive - start
  Gen(map  (nonNegativeLessThan(dist)) (_+1))
}

def sameParity(from: Int, to: Int): Gen[(Int,Int)] =  (choose(from,to) map2 ( choose(from,to))) ((_,_)) flatMap ((t)=>(t._1+t._2%2==0) match {
  case true => unit(t)
  case false => sameParity(from,to)
})


}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}




case class Simple(seed: Long) extends RNG {
      def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = Simple(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
      }
    }


//Tests
//  val intList = Gen.listOf(Gen.choose(0,100))
//  val onesList = Gen.listOf(Gen.choose(1,1))
//  def sum (l:List[Int]):Int =  l . foldRight( 0) (_ + _)
// val orderInvariant = forAll(intList) ( sum (l) == sum (l.reverse))
// val sumLengthProp = forAll (onesList) (sum(l) == length(l))
//maximum of list int
 //val orderInvariantProp = forAll(intList) ( max (l) == max (l.reverse))
 //val associativeProp = forAll(intList) (max (l) == max(  list(max(split(l, l.len/2)(0)) , max(split(l, l.len/2)(0)))))




}

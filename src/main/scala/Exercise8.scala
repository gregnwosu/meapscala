import org.scalacheck.Prop.forAll
import org.scalacheck.Gen


class Exercise8 {
def listOf[A](a: Gen[A]): Gen[List[A]]
def forAll[A](l:Gen[List[A]]) (f : A => Boolean):Boolean

trait Prop { 
  def &&(p: Prop): Prop  = Prop(this.success && p.success)
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}


case class Gen[A](sample: State[RNG,A])

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

//notes use a random number generator internally 

def choose(start: Int, stopExclusive: Int): Gen[Int]  = {
  val dist = stopExclusive - start
  map (_+1) nonNegativeLessThan(dist)
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
  val intList = Gen.listOf(Gen.choose(0,100))
  val onesList = Gen.listOf(Gen.choose(1,1))
  def sum (l:List[Int]):Int =  l . foldRight( 0) (_ + _)
 val orderInvariant = forAll(intList) ( sum (l) == sum (l.reverse))
 val sumLengthProp = forAll (onesList) (sum(l) == length(l))
//maximum of list int
 val orderInvariantProp = forAll(intList) ( max (l) == max (l.reverse))
 val associativeProp = forAll(intList) (max (l) == max(  list(max(split(l, l.len/2)(0)) , max(split(l, l.len/2)(0)))))




}

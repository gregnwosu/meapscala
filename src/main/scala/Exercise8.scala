object Exercise8 {


  def main(args: Array[String]):Unit  = {
     Prop.run(maxProp)
   }


  def listOfGen[A](g: Gen[A]): Gen[List[A]] = Gen(sequence (List.fill(100)(g.sample)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
  s"generated an exception: ${e.getMessage}\n" +
  s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

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


   //GEN
   case class Gen[A](sample: State[RNG,A]){
    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen((s:RNG) =>  {
      val (a,s1) = sample (s)
      val f2 = f(a).sample
      f2(s1)
    })

    def map[B](f: A => B): Gen[B]  = this flatMap ((a:A) => Gen.unit(f(a)))

    def map2[B,C](rb:Gen[B])(f: (A,B)=>C): Gen[C] = flatMap ((a:A) => rb map  ((b:B) => f(a,b) ))

    def listOf(size: Gen[Int]): Gen[List[A]] = size flatMap ((sz:Int)=>listOfNStatic(this)(sz))


    //aha now we have flatmap
    def listOfNFor(size: Gen[Int]): Gen[List[A]] =
      for{
        sz <- size
      l  <- listOfNStatic(this) (sz)
      }yield(l)


    def unsized: SGen[A]  = SGen( _ => this)





  }

   case class SGen[A](forSize: Int => Gen[A]){

  }


  

   def boolean: Gen[Boolean] = Gen(map (((x:RNG) => x.nextInt):State[RNG,Int])  (_%2==0))

   def listOfNStatic[A](g: Gen[A])(n: Int): Gen[List[A]]  = Gen(sequence (List.fill(n)(g.sample)))



   type TestCases = Int
   type FailedCase = String
   type SuccessCount = Int
  type Result = Option[(FailedCase, SuccessCount)]


   case class Simple(seed: Long) extends RNG {
      def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = Simple(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
      }

     def nextDouble: (Double, RNG) = {
       val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
       val nextRNG = Simple(newSeed)
       val n = (newSeed >>> 16).toInt
        (n, nextRNG)
     }
   }

   trait RNG {
     def nextInt: (Int, RNG)
     def nextDouble: (Double, RNG)
   }



   def randomStream[A](g:Gen[A])(rng:RNG):Stream[A] = {
     val (v,nextrng) = g.sample(rng)
     scala.collection.immutable.Stream.cons (v,(randomStream(g)(rng)))
   }


   // def forAll[A](as:Gen[A]) (f : A => Boolean):Prop
   // = Prop {
   //   (max,n,rng,sd) => randomStream(as)(rng) .zip (Stream.from(0)).take(n).map{
   //     case (a,i) => try {
   //       if (f(a)) None else Some((a.toString, i))
   //     } catch { case e: Exception => Some ((buildMsg(a,e),i))}
   //   }.find(_.isDefined).getOrElse(None)
   // }



   //  def check: Either[(FailedCase, SuccessCount), SuccessCount]



   object Gen{
     def unit[A](a: => A):Gen[A] = Gen((s:RNG) => (a,s))
     def nonNegativeInt(rng: RNG):(Int,RNG)={
       val  (res, rng2) = rng.nextInt
       if (res == Int.MinValue)
         nonNegativeInt(rng2)
       else
         (Math.abs(res), rng2)
     }

     def doubleGen():Gen[Double] = Gen((s:RNG) =>(s.nextDouble))



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

     def sameParityFor(from: Int, to: Int): Gen[(Int,Int)] =
       for {
         v1 <- choose(from, to )
         v2 <- choose(from, to )
         t <-
         if (v1+v2%2==0)
         unit((v1,v2)):Gen[(Int,Int)]
         else
         sameParityFor(from, to)
       }yield(t)



   }


   def genAToOptA[A](g:Gen[A]):Gen[Option[A]] = g map ((Some(_)))


   def genString(n:Int):Gen[String] = for{
     lnum <- listOfNStatic(Gen.choose(32,126)) (n)
   } yield((lnum map (_.toChar)).mkString)




   def union[A](g1: Gen[A] , g2 : Gen[A]):Gen[A]  = for{
     n <- Gen.choose(0,1)
     g  <- if (n==0) g1 else g2
   }yield(g)


   def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A]  =
     for{
       p <- Gen.doubleGen
       g <-  if (g1._2 < g2._2)
       if (g1._2 < p)
       g1._1
       else
       g2._1
       else
       if (g2._2 < p)
       g2._1
       else
       g1._1

     }yield(g)





   def genOptAToA[A](g:Gen[Option[A]]):Gen[A] = for {
     optA <- g
     a <- optA match {
       case None => genOptAToA(g)
       case Some(y) => Gen.unit(y):Gen[A]
     }
   }yield(a)

   object Prop {
     type FailedCase = String
     type SuccessCount = Int


     def run( p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG =
       Simple(System.currentTimeMillis), sd:Char='L'): Unit =
       p.run(maxSize, testCases, rng, sd) match {
         case Some((msg, n)) => println(s"! Falsified after $n passed tests:\n $msg") case None => println(s"+ OK, passed $testCases tests.")
       }

   }



  type MaxSize = Int


  val smallInt = Gen.choose(-10,10)
  val maxProp = forAll(listOf(smallInt) ) ({ (l) =>{
       val max = l.max
       !l.exists(_ > max)
     }
   })




   case class Prop(run: (Int, TestCases,RNG, Char) => Result ){
     def && (p: Prop): Prop   = Prop((numcases:Int, tc:TestCases, rng:RNG, tag:Char) => this.run(numcases, tc, rng, 'L') match{
       case None => p.run(numcases,tc,rng,'R')
       case report => report
     })

     def || (p: Prop): Prop   = Prop((numcases:Int, tc:TestCases, rng:RNG, tag:Char) => this.run(numcases, tc, rng,'L') match{
       case None => None
      case report => p.run(numcases,tc,rng,'R')
     })





     // def check : Boolean
   }


  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen((n:Int)=>Gen(sequence (List.fill(n)(g.sample))))
  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop =
     Prop { (max,n,rng,sd) =>
       val casesPerSize = (n + (max - 1)) / max
       val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(SGen((x => g(x)))  )(f))
       val prop:Prop = props.map(p => Prop ( (max, _, rng,sd) => p.run(max, casesPerSize, rng,sd) )).toList.reduce(_ && _)
       prop run(max,n,rng,sd)
     }


  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
       forAll(g.forSize(_))(f)


  







   // case class Simple(seed: Long) extends RNG {
   //       def nextInt: (Int, RNG) = {
   //         val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
   //         val nextRNG = Simple(newSeed)
   //         val n = (newSeed >>> 16).toInt
   //         (n, nextRNG)
   //       }
   //     }
   //Tests
   //val intList = Gen.listOf(Gen.choose(0,100))
   //val onesList = Gen.listOf(Gen.choose(1,1))
   //def sum (l:List[Int]):Int =  l . foldRight( 0) (_ + _)
   //val orderInvariant = forAll(intList) ( sum (l) == sum (l.reverse))
   //val sumLengthProp = forAll (onesList) (sum(l) == length(l))
   //maximum of list int
   //val orderInvariantProp = forAll(intList) ( max (l) == max (l.reverse))
   //val associativeProp = forAll(intList) (max (l) == max(  list(max(split(l, l.len/2)(0)) , max(split(l, l.len/2)(0)))))



   


}


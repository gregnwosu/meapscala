
object Hello{

  def main(args: Array[String]) :Unit  = {
    println("Hello World")
    println("this is a test")
  }

 def partial[A,B,C](a:A , f: (A,B)=>C): B => C =
 (b:B) => f (a, b) 

}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/***
  * Euler totient function implementation in functional style phi(n).
  *
  */

object EulterTotientFunction {

  @tailrec
  def gcd(a:Int,b:Int):Int={
    (a,b) match {
      case (a,0)=> a
      case (a,b)=> gcd(b,a%b)
    }
  }

  def main(args: Array[String]): Unit = {
    val n=12
    val res = (1 to n).count(gcd(n,_)==1)
    print(res)
  }
}

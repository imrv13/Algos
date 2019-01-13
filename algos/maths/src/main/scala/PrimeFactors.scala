object PrimeFactors {
  def main(args: Array[String]): Unit = {

  }
  def pFactors(a:Int): Unit ={

    def evenFact(x:Double): Double ={
      (x%2,math.floor(x/2)) match {
        case (0,_)=> evenFact(math.floor(x/2))
        case (_,_)=>math.floor(x/2)
      }
    }
    (1 to a).map(evenFact(_))

  }
}

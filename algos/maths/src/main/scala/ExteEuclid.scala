object ExteEuclid {
  /***
    * ax+by=d
    *
    * @param args
    */
  def main(args: Array[String]): Unit = {

   val res= egcd(10,5,1,1)
    print(res)


  }

  def egcd(a:Int,b:Int,x:Int,y:Int): (Int,Double,Double) ={

    val (g,x11,y11)=(a,b) match {
      case (a,0)=>{
        return (a,1,0)
      }
      case (a,b)=> egcd(b,a%b,x,y)
    }

    val x1=y11
    val y1=(x11-math.floor(a/b)*y11)

    return (g,x1,y1)

  }
}

/***
  * merge two arrays
  */

object merge {
  def main(args: Array[String]): Unit = {

    val a=List(List(1,2,3),List(0,6,8),List(10,11,100))

   val l= a.foldLeft(List[Int]())(merge(_,_))
    print(l)


  }

  def merge(a:List[Int],b:List[Int]): List[Int] ={
    (a,b) match {
      case (List(),x::xs)=>b
      case (x::xs,List())=>a
      case (x::xs,y::ys)=>if(x<y) x::merge(xs,b) else y::merge(a,ys)
      case (List(),List())=>List()
    }
  }
}



/**
  * Functional Inplementaion of graphs
  * @param src
  * @param tget
  * @param weight
  * @tparam N
  * @tparam E
  */
case class Edge[N,E](src:Node[N,E],tget:Node[N,E],weight:E)

class Node[N,E](var value:N = null.asInstanceOf[N]){

  var inEdges:List[Edge[N,E]] = Nil

  var outEdges:List[Edge[N,E]] = Nil

  def succ:List[Node[N,E]] = outEdges.map(_.tget)

  def pdec:List[Node[N,E]] = inEdges.map(_.src)

  def hop(from:N):Option[Node[N,E]] = graphyByDepth.find(_.value==from)


  def connect(from: N,to:N,via:E):(Node[N,E],Node[N,E])={

    val froNode : Node[N,E] = if (value==null) {value=from;  this} else  hop(from) match {
      case Some(g) => g
      case None => new  Node(from)
    }

    val toNode : Node[N,E] = hop(to) match {
      case Some(g) => g
      case None => new Node(to)
    }

    froNode.outEdges =  new Edge(froNode,toNode,via) :: froNode.outEdges
    toNode.inEdges =  new Edge(froNode,toNode,via) :: toNode.inEdges

    (froNode,toNode)
  }





  def graphyByDepth:List[Node[N,E]]={

    def loop(g: Node[N,E], s: Set[Node[N,E]]): Set[Node[N,E]] =
      if (!s(g)) {
        val ss = g.succ.foldLeft(s + g)((acc, gg) => loop(gg, acc))

        g.pdec.foldLeft(ss)((acc, gg) => loop(gg, acc))
      } else s

    loop(this, Set()).toList

  }

  override def equals(obj: Any): Boolean = obj match {
    case g:Node[_,_] => g.value == value
    case _ => false
  }

  override def toString: String = "Graph(" + value + ")"



}

object Node{


  def main(args: Array[String]): Unit = {

    val a=new Node[String,Int]

    a.connect("A","B",10)
    a.connect("B","c",20)
    print(a.graphyByDepth)


  }

}




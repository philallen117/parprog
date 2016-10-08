// Assuming out.length >= inp.length + 1
def scanLeft[A](inp: Array[A], a0: A, out: Array[A], f: (A, A)  => A): Unit = {
  out(0) = a0
  var acc: A = a0
  var i: Int = 0; var j: Int = 1
  while (i < inp.length) {
    acc = f(acc, inp(i))
    out(j) = acc
    i += 1; j += 1
  }
}

val out = Array(0, 0, 0, 0)
scanLeft(Array[Int](1, 5, 3), 100, out, (acc: Int, e: Int) => acc + e)
out.toList

def reduceSeg1[A](inp: Array[A], left: Int, right: Int, a0: A, f: (A, A) => A): A = ???

def mapSeg[A,B](inp: Array[A], left: Int, right: Int, fi: (Int, A) => B,
          out: Array[A]): Unit = ???

def scanLeft1[A](inp: Array[A], a0: A, out: Array[A], f: (A, A)  => A): Unit = {
  def fi(i: Int, a: A) = reduceSeg1(inp, 0, i, a0, f)
  mapSeg(inp, 0, inp.length, fi, out)
  val len = inp.length
  out(len) = f(out(len - 1), inp(len - 1))
}

abstract class Tree[A]
case class Leaf[A](a: A) extends Tree[A]
case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

abstract class TreeRes[A] { val res: A }
case class LeafRes[A](res: A) extends TreeRes[A]
case class NodeRes[A](l: TreeRes[A], res: A, r: TreeRes[A]) extends TreeRes[A]

def upsweep[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
  case Leaf(a) => LeafRes(a)
  case Node(l, r) =>
    val (al, ar) = common.parallel(upsweep(l, f), upsweep(r, f))
    NodeRes(al, f(al.res, ar.res), ar)
}

def downsweep[A](tr: TreeRes[A], a0: A, f: (A, A) => A): Tree[A] = tr match {
  case LeafRes(res) => Leaf(f(a0, res))
  case NodeRes(ltr, _, rtr) =>
    val (lt, rt) = common.parallel(
      downsweep[A](ltr, a0, f),
      downsweep[A](rtr, f(a0, ltr.res), f)
    )
    Node[A](lt, rt)
}

def scanLeft1[A](t: Tree[A], a0: A, f: (A, A) => A): Tree[A] = {
  val res = upsweep(t, f)
  val t = downsweep(res, a0, f)
  Node(Leaf(a0), t)
}
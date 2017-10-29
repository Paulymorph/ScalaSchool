package lectures.collections

import lectures.collections.CherryTree.{Node, Node1, Node2}

import scala.collection.generic._
import scala.collection.{GenTraversableOnce, LinearSeq, LinearSeqOptimized, mutable}

sealed trait CherryTree[+T] extends LinearSeq[T]
  with LinearSeqOptimized[T, CherryTree[T]]
  with GenericTraversableTemplate[T, CherryTree]
  with Product with Serializable {

  def append[S >: T](x: S): CherryTree[S]

  def prepend[S >: T](x: S): CherryTree[S]

  def concat[S >: T](xs: CherryTree[S]): CherryTree[S]

  override def init: CherryTree[T]

  override def tail: CherryTree[T]

  override def foldLeft[B](z: B)(op: (B, T) => B): B

  override def toString(): String = super.toString()

  override def companion = CherryTree

  override def stringPrefix: String = "CherryTree"


  // If we have a default builder, there are faster ways to perform some operations
  @inline private[this] def isDefaultCBF[A, B, That](bf: CanBuildFrom[CherryTree[A], B, That]): Boolean = bf eq CherryTree.ReusableCBF

  override def :+[B >: T, That](elem: B)(implicit bf: CanBuildFrom[CherryTree[T], B, That]) =
    if (isDefaultCBF(bf)) append(elem).asInstanceOf[That] else super.:+(elem)

  override def +:[B >: T, That](elem: B)(implicit bf: CanBuildFrom[CherryTree[T], B, That]) =
    if (isDefaultCBF(bf)) prepend(elem).asInstanceOf[That] else super.:+(elem)

  override def ++[B >: T, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[CherryTree[T], B, That]) =
    if (isDefaultCBF(bf)) concat(that.asInstanceOf[CherryTree[B]]).asInstanceOf[That] else super.++(that)
}

case object CherryNil extends CherryTree[Nothing] {
  override def head = throw new NoSuchElementException("head of empty CherryList")

  override def last = throw new NoSuchElementException("last of empty CherryList")

  override def tail = throw new UnsupportedOperationException("tail of empty CherryList")

  override def init = throw new UnsupportedOperationException("init of empty CherryList")

  override def foreach[U](f: (Nothing) => U) = ()

  override def append[S >: Nothing](x: S): CherryTree[S] = CherrySingle(x)

  override def size = 0

  override def isEmpty = true

  override def prepend[S >: Nothing](x: S) = append(x)

  override def concat[S >: Nothing](xs: CherryTree[S]) = xs

  override def apply(n: Int): Nothing =
    throw new IndexOutOfBoundsException(
      s"trying to get an element #$n from an empty collection")

  override def foldLeft[B](z: B)(op: (B, Nothing) => B): B = z
}

final case class CherrySingle[+T](x: T) extends CherryTree[T] {
  override def head = x

  override def tail = CherryNil

  override def init = CherryNil

  override def foreach[U](f: T => U) = f(x)

  def append[S >: T](y: S) = CherryBranch(Node1(x), CherryNil, Node1(y))

  override def size = 1

  override def isEmpty = false

  override def prepend[S >: T](x: S) = CherryBranch(Node1(x), CherryNil, Node1(this.x))

  override def concat[S >: T](xs: CherryTree[S]) = xs.prepend(x)

  override def apply(n: Int): T =
    if (n == 0)
      x
    else
      throw new IndexOutOfBoundsException(s"trying to get the element #$n from a collection with one element")

  override def foldLeft[B](z: B)(op: (B, T) => B): B = op(z, x)
}

final case class CherryBranch[+T](left: Node[T], inner: CherryTree[Node2[T]], right: Node[T]) extends CherryTree[T] {
  override def head = left match {
    case Node1(x) => x
    case Node2(x, _) => x
  }

  override def tail = left match {
    case Node1(_) => inner match {
      case CherryNil => right match {
        case Node1(x) => CherrySingle(x)
        case Node2(x, y) => CherryBranch(Node1(x), CherryNil, Node1(y))
      }
      case tree => CherryBranch(tree.head, tree.tail, right)
    }
    case Node2(_, x) => CherryBranch(Node1(x), inner, right)
  }

  override def foreach[U](f: T => U) = {
    left.foreach(f)
    inner.foreach(_.foreach(f))
    right.foreach(f)
  }

  def append[S >: T](x: S) = right match {
    case Node1(y) => CherryBranch(left, inner, Node2(y, x))
    case n: Node2[S] => CherryBranch(left, inner.append(n), Node1(x))
  }

  override def size = left.size + inner.size * 2 + right.size

  override def isEmpty = false

  override def prepend[S >: T](x: S) = left match {
    case Node1(y) => CherryBranch(Node2(x, y), inner, right)
    case n: Node2[S] => CherryBranch(Node1(x), inner.prepend(n), right)
  }

  override def concat[S >: T](xs: CherryTree[S]) =
  //    if (this.size > xs.size)
  //      xs.foldLeft(this)((acc, i) => acc.append(i))
  //    else
    this.foldRight(xs)((i, acc) => acc.prepend(i))

  override def init = right match {
    case Node1(_) =>
      inner match {
        case CherryNil => left match {
          case Node1(x) => CherrySingle(x)
          case Node2(x, y) => CherryBranch(Node1(x), CherryNil, Node1(y))
        }
        case _ => CherryBranch(left, inner.init, inner.last)
      }
    case Node2(x, y) => CherryBranch(left, inner, Node1(x))
  }

  override def apply(n: Int): T = n match {
    case n if n < 0 || n >= size =>
      throw new IndexOutOfBoundsException(s"accessing $n-th element in a tree of size $size")

    case n if n < left.size =>
      left match {
        case Node1(x) => x
        case Node2(x, y) => if (n == 0) x else y
      }

    case n if n < size - right.size =>
      val newIndex = n - left.size
      val searchNode = inner(newIndex / 2)
      if (newIndex % 2 == 0)
        searchNode.x
      else
        searchNode.y

    case _ =>
      right match {
        case Node1(x) => x
        case Node2(x, y) => if (n == size - 1) y else x
      }
  }

  override def foldLeft[B](z: B)(op: (B, T) => B): B = {
    val leftRes = left.foldLeft(z)(op)
    val expandOp = (acc: B, node: Node2[T]) => node.foldLeft(acc)(op)
    val innerRes = inner.foldLeft(leftRes)(expandOp)
    right.foldLeft(innerRes)(op)
  }
}


object CherryTree extends SeqFactory[CherryTree] {

  def newBuilder[T]: mutable.Builder[T, CherryTree[T]] = new CherryTreeBuilder[T]

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, CherryTree[A]] =
    ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]

  sealed trait Node[+T] {
    def foreach[U](f: T => U): Unit

    def size: Int

    def foldLeft[B](z: B)(op: (B, T) => B): B
  }

  final case class Node1[+T](x: T) extends Node[T] {
    override def foreach[U](f: (T) => U): Unit = f(x)

    def size = 1

    override def foldLeft[B](z: B)(op: (B, T) => B): B = op(z, x)
  }

  final case class Node2[+T](x: T, y: T) extends Node[T] {
    def foreach[U](f: (T) => U): Unit = {
      f(x)
      f(y)
    }

    def size = 2

    override def foldLeft[B](z: B)(op: (B, T) => B): B =
      op(op(z, x), y)
  }

  private class CherryTreeBuilder[T]() extends mutable.Builder[T, CherryTree[T]] {
    private[this] var coll: CherryTree[T] = CherryNil

    def +=(elem: T) = {
      coll = coll.append(elem);
      this
    }

    def clear(): Unit = coll = CherryNil

    def result(): CherryTree[T] = coll
  }

}


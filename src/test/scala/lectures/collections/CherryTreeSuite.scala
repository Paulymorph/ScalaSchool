package lectures.collections

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class CherryTreeSuite extends FlatSpec with PropertyChecks with Matchers {
  "Cherry tree" should "append element" in forAll { (x: Int, xs: Vector[Int]) =>
    val tree = CherryTree(xs: _*)
    tree.append(x) shouldBe CherryTree(xs :+ x: _*)
  }

  it should "prepend element" in forAll { (x: Int, xs: Vector[Int]) =>
    val tree = CherryTree(xs: _*)
    tree.prepend(x) shouldBe CherryTree(x +: xs: _*)
  }

  it should "get tail" in forAll { xs: Vector[Int] =>
    val tree = CherryTree(xs: _*)
    if (xs.isEmpty) an[UnsupportedOperationException] should be thrownBy tree.tail
    else xs.tail shouldBe CherryTree(xs.tail: _*)
  }

  it should "get head" in forAll { xs: Vector[Int] =>
    val tree = CherryTree(xs: _*)
    if (xs.isEmpty) an[NoSuchElementException] should be thrownBy tree.head
    else xs.head shouldBe xs.head
  }

  it should "get init" in forAll { xs: Vector[Int] =>
    val tree = CherryTree(xs: _*)
    if (xs.isEmpty) an[UnsupportedOperationException] should be thrownBy tree.init
    else xs.tail shouldBe CherryTree(xs.tail: _*)
  }

  it should "get last" in forAll { xs: Vector[Int] =>
    val tree = CherryTree(xs: _*)
    if (xs.isEmpty) an[NoSuchElementException] should be thrownBy tree.last
    else xs.head shouldBe xs.head
  }

  it should "get element by index" in forAll { (xs: Vector[Int], i: Int) =>
    val tree = CherryTree(xs: _*)
    if (i < xs.size && i >= 0) {
      tree(i) shouldBe xs(i)
    } else an[IndexOutOfBoundsException] should be thrownBy tree(i)
  }

  it should "concat elements" in forAll { (xs: List[Int], ys: List[Int]) =>
    CherryTree(xs: _*) concat CherryTree(ys: _*) shouldBe CherryTree(xs ++ ys: _*)
  }

  it should "get correct size" in forAll { (xs: Vector[Int]) =>
    CherryTree(xs: _*).size shouldBe xs.size
  }

  it should "fold left correctly" in forAll { (x: List[Int], i: Int) =>
    CherryTree(x: _*).foldLeft(i)(_ - _) shouldBe x.foldLeft(i)(_ - _)
    CherryTree(x: _*).foldLeft(i)(_ + _) shouldBe x.foldLeft(i)(_ + _)
  }
}

package org.spbsu.mkn.scala

import org.scalatest.funsuite.AnyFunSuite
import org.spbsu.mkn.scala.MyGenericList.{fromSeq, size, sum}

class MyGenericListTest extends AnyFunSuite {

  test("head") {
    assert(fromSeq(Seq(1, 2, 3)).head == 1)
    assert(fromSeq(Seq(1)).head == 1)
    assertThrows[UnsupportedOperationException](fromSeq(Seq()).head)
  }

  test("tail") {
    assert(fromSeq(Seq(1, 2, 3)).tail == fromSeq(Seq(2, 3)))
    assert(fromSeq(Seq(1)).tail == MyNil)
  }

  test("drop") {
    assert(fromSeq(Seq(1, 2, 3)).drop(0) == fromSeq(Seq(1, 2, 3)))
    assert(fromSeq(Seq(1, 2, 3)).drop(2) == fromSeq(Seq(3)))
    assert(fromSeq(Seq(1, 2, 3)).drop(3) == MyNil)
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1, 2, 3)).drop(10))
  }

  test("take") {
    assert(fromSeq(Seq(1, 2, 3)).take(0) == MyNil)
    assert(fromSeq(Seq(1, 2, 3)).take(2) == fromSeq(Seq(1, 2)))
    assert(fromSeq(Seq(1, 2, 3)).take(3) == fromSeq(Seq(1, 2, 3)))
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1, 2, 3)).take(10))
  }

  trait Animal {
    val name: String
    final val age = 1
  }
  case class Cat(override val name: String) extends Animal
  case class Dog(override val name: String) extends Animal

  test("map") {
    assert(MyNil.map[Int]((_: Int) * 2) == MyNil)
    assert(fromSeq(Seq(1, 2, 3)).map[Int](_ * 2) == fromSeq(Seq(2, 4, 6)))
    assert(fromSeq(Seq(1, 2, 3)).map[Int](identity) == fromSeq(Seq(1, 2, 3)))

    assert(fromSeq(Seq('a', 'b', 'c')).map[Int](x => 26 * (x - 'a')) == fromSeq(Seq(0, 26, 52)))
    assert(fromSeq(Seq(Dog("Tuzik"), Cat("Alice"))).map[String](x => x.name) == fromSeq(Seq("Tuzik", "Alice")))
    assert(fromSeq(Seq[Animal](Dog("Bobik"), Dog("Tuzik"))).map[Int](x => x.age) == fromSeq(Seq(1, 1)))
  }

  test("size") {
    assert(size(MyNil) == 0)
    assert(size(fromSeq(Seq(1, 2, 3))) == 3)
  }

  test("sum") {
    assertThrows[UnsupportedOperationException](sum[Int](MyNil))
    assert(sum(fromSeq(Seq(1, 2, 3))) == 6)
    assert(sum(fromSeq(Seq(1))) == 1)

    assert(sum(fromSeq(Seq(' ', '0'))) == 'P')
  }
}
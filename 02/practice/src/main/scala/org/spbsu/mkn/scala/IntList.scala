package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._
import scala.annotation.tailrec

sealed trait IntList {
  def head: Int

  def tail: IntList

  def drop(n: Int): IntList

  def take(n: Int): IntList

  def map(f: Int => Int): IntList

  def ::(elem: Int): IntList = IntCons(elem, this)
}

case object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq(seq: Seq[Int]): IntList = {
    if (seq.isEmpty) IntNil
    else IntCons(seq.head, fromSeq(seq.drop(1)))
  }

  def sum(intList: IntList): Int = {
    intList match {
      case IntNil => undef
      case _ => foldLeft(intList, 0){_ + _}
    }
  }

  def size(intList: IntList): Int = {
    intList match {
      case IntNil => 0
      case IntCons(_, list) => 1 + size(list)
    }
  }

  // extra task: implement sum using foldLeft
  @tailrec
  def foldLeft[T](intList: IntList, initValue: T)(f: (T, Int) => T): T = {
    intList match {
      case IntNil => initValue
      case IntCons(x, list) => foldLeft(list, f(initValue, x))(f)
    }
  }
}

case object IntNil extends IntList {
  override def head: Int = undef

  override def tail: IntList = IntNil

  override def drop(n: Int): IntList = {
    if (n == 0) IntNil
    else undef
  }

  override def take(n: Int): IntList = {
    if (n == 0) IntNil
    else undef
  }

  override def map(f: Int => Int): IntList = IntNil
}

case class IntCons(x: Int, list: IntList) extends IntList {
  override def head: Int = x

  override def tail: IntList = list

  override def drop(n: Int): IntList = {
    if (n == 0) this
    else list.drop(n - 1)
  }

  override def take(n: Int): IntList = {
    if (n == 0) IntNil
    else IntCons(x, list.take(n - 1))
  }

  override def map(f: Int => Int): IntList = IntCons(f(x), list.map(f))
}
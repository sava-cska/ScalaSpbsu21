package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._

import java.util.Comparator
import scala.annotation.tailrec

sealed trait MyGenericList[+T] {
  def head: T

  def tail: MyGenericList[T]

  def drop(n: Int): MyGenericList[T]

  def take(n: Int): MyGenericList[T]

  def map[S](f: T => S): MyGenericList[S]

  def ::[S >: T](elem: S): MyGenericList[S] = MyCons(elem, this)
}

case object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq[T](seq: Seq[T]): MyGenericList[T] = seq.foldRight[MyGenericList[T]](MyNil) { (x, list) => MyCons(x, list) }

  def size[T](myList: MyGenericList[T]): Int = {
    myList match {
      case MyNil => 0
      case MyCons(_, list) => 1 + size(list)
    }
  }

  def sum[T: Numeric](myList: MyGenericList[T]): T = {
    myList match {
      case MyNil => undef
      case _ => foldLeft(myList, Numeric[T].zero) { Numeric[T].plus }
    }
  }

  @tailrec
  def foldLeft[T, S](myList: MyGenericList[T], initValue: S)(f: (S, T) => S): S = {
    myList match {
      case MyNil => initValue
      case MyCons(x, list) => foldLeft(list, f(initValue, x))(f)
    }
  }

  def sort[T](list: MyGenericList[T])(implicit comparator: Comparator[T]): MyGenericList[T] = ???
}

case object MyNil extends MyGenericList[Nothing] {
  override def head: Nothing = undef

  override def tail: MyGenericList[Nothing] = MyNil

  override def drop(n: Int): MyGenericList[Nothing] = {
    n match {
      case 0 => MyNil
      case _ => undef
    }
  }

  override def take(n: Int): MyGenericList[Nothing] = {
    n match {
      case 0 => MyNil
      case _ => undef
    }
  }

  override def map[S](f: Nothing => S): MyGenericList[S] = MyNil
}

case class MyCons[T](x: T, list: MyGenericList[T]) extends MyGenericList[T] {
  override def head: T = x

  override def tail: MyGenericList[T] = list

  override def drop(n: Int): MyGenericList[T] = {
    n match {
      case 0 => this
      case _ => list.drop(n - 1)
    }
  }

  override def take(n: Int): MyGenericList[T] = {
    n match {
      case 0 => MyNil
      case _ => MyCons(x, list.take(n - 1))
    }
  }

  override def map[S](f: T => S): MyGenericList[S] = MyCons(f(x), list.map(f))
}

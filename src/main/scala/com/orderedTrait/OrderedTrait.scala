package com.orderedTrait

trait Ordered[A] {
  def compare(that: A): Int
  def < (that: A): Boolean = (this compare that) < 0
  def > (that: A): Boolean = (this compare that) > 0
  def <= (that: A): Boolean = (this compare that) <= 0
  def >= (that: A): Boolean = (this compare that) >= 0
  def compareTo(that: A): Int = compare(that)
}

trait Set[A <: Ordered[A]] {
  def incl(x: A): Set[A]
  def contains(x: A): Boolean
}

case class Empty[A <: Ordered[A]]() extends Set[A] {
  def contains(x: A): Boolean = false
  def incl(x: A): Set[A] = NonEmpty(x, Empty[A](), Empty[A]())
}

case class NonEmpty[A <: Ordered[A]](value: A, leftNode: Set[A], rightNode: Set[A]) extends Set[A] {

  def contains(x: A): Boolean =
    if (x < value) leftNode.contains(x)
    else if (x > value) rightNode.contains(x)
    else true

  def incl(x: A): Set[A] =
    if (x < value) NonEmpty(value, leftNode.incl(x), rightNode)
    else if (x > value) NonEmpty(value, leftNode, rightNode.incl(x))
    else this
}

case class Input(value: Int) extends Ordered[Input] {
  def compare(that: Input): Int =
    if (this.value < that.value) -1
    else if (this.value > that.value) 1
    else 0
}

object RUn extends App {
  val tree = Empty[Input].incl(Input(1)).incl(Input(2))
  println(tree.contains(Input(1)))
}

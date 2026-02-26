package caos.common

import scala.annotation.targetName

case class Multiset[A](data: Map[A, Int] = Map.empty):
  override def toString: String =
    this.data.flatMap{ case (element, count) =>
      List.fill(count)(element.toString)
    }.mkString(", ")
  end toString

  def isEmpty: Boolean =
    this.data.isEmpty
  end isEmpty

  def contains(element: A): Boolean =
    this.data.contains(element)
  end contains

  @targetName("add")
  def +(element: A): Multiset[A] =
    val updatedCount = this.data.getOrElse(element, 0) + 1
    Multiset(this.data + (element -> updatedCount))
  end +

  @targetName("concat")
  def ++(multisetB: Multiset[A]): Multiset[A] =
    Multiset(
      (this.data.keySet ++ multisetB.data.keySet).map( element =>
        val countA = this.data.getOrElse(element, 0)
        val countB = multisetB.data.getOrElse(element, 0)
        element -> (countA + countB)
      ).toMap
    )
  end ++


  @targetName("delete")
  def -(act:A): Multiset[A] =
    data.get(act) match
      case Some(v) if v>1 =>
        Multiset(data + (act -> (v-1)))
      case _ =>
        Multiset(data - act)

  def included(other: Multiset[A]): Boolean =
    data.forall(a1 => other.data.get(a1._1).exists(_>=a1._2))
  @targetName("exclude")
  def --(multisetB: Multiset[A]): Multiset[A] =
    val updatedDataA = this.data.map{ case (elementA, countA) =>
      val updatedCountA = countA - multisetB.data.getOrElse(elementA, 0)
      (elementA, updatedCountA)
    }.filter{ case (_, updatedCountA) =>
      updatedCountA > 0
    }
    Multiset(updatedDataA)
  end --


object Multiset:
  //    def apply[A](m:Map[A,Int]) = new Multiset[A]:
  //      data = m
  def apply[A]() = new Multiset[A](Map())
